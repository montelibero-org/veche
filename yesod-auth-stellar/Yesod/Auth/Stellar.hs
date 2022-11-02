{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.Stellar
    (
    -- * Auth plugin
      authStellar
    , Config (..)
    ) where

import Debug.Trace

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Function ((&))
import Data.String (IsString)
import Data.Text (Text, strip)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (notFound404)
import Network.ONCRPC.XDR (lengthArray, unLengthArray, xdrDeserialize,
                           xdrSerialize)
import Network.Stellar.Builder (buildWithFee, tbMemo, tbOperations,
                                transactionBuilder)
import Network.Stellar.Builder qualified as Stellar
import Network.Stellar.Keypair (decodePublicKey)
import Network.Stellar.Network (publicNetwork, testNetwork)
import Network.Stellar.Network qualified as Stellar
import Network.Stellar.TransactionXdr (ManageDataOp (ManageDataOp),
                                       Memo (Memo'MEMO_TEXT),
                                       MuxedAccount (MuxedAccount'KEY_TYPE_ED25519, MuxedAccount'KEY_TYPE_MUXED_ED25519),
                                       Operation (Operation),
                                       OperationBody (OperationBody'MANAGE_DATA),
                                       PublicKey (PublicKey'PUBLIC_KEY_TYPE_ED25519),
                                       TransactionEnvelope (TransactionEnvelope'ENVELOPE_TYPE_TX, TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP, TransactionEnvelope'ENVELOPE_TYPE_TX_V0),
                                       TransactionV0 (TransactionV0),
                                       TransactionV0Envelope (TransactionV0Envelope),
                                       TransactionV1Envelope (TransactionV1Envelope))
import Network.Stellar.TransactionXdr qualified as XDR
import Network.URI (escapeURIString, isReserved)
import Servant.Client (BaseUrl, ClientError (FailureResponse),
                       ResponseF (Response, responseStatusCode), mkClientEnv,
                       runClientM)
import Yesod.Auth (Auth, AuthHandler, AuthPlugin (AuthPlugin), Creds (Creds),
                   Route (PluginR), authLayout, setCredsRedirect)
import Yesod.Auth qualified
import Yesod.Core (HandlerFor, HandlerSite, Html, MonadHandler, RenderMessage,
                   TypedContent, WidgetFor, badMethod, getRouteToParent,
                   invalidArgs, julius, liftHandler, liftIO, logErrorS,
                   lookupGetParam, notAuthenticated, toTypedContent, toWidget,
                   whamlet)
import Yesod.Form (AForm, FormMessage, FormResult (FormSuccess), aopt, areq,
                   fsName, textField, textareaField, unTextarea)
import Yesod.Form qualified as Yesod
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap5)

-- project
import Stellar.Horizon.Client (Account (Account),
                               Operation (OperationManageData), Signer (Signer),
                               Transaction (Transaction), getAccount)
import Stellar.Horizon.Client qualified as Stellar
import Stellar.Simple (Memo (MemoText))

pluginName :: Text
pluginName = "stellar"

pluginRoute :: Route Auth
pluginRoute = PluginR pluginName []

data Config app = Config
    { horizon :: BaseUrl
    , getVerifyKey :: Stellar.Address -> HandlerFor app Text
    , checkAndRemoveVerifyKey :: Stellar.Address -> Text -> HandlerFor app Bool
    }

authStellar :: Config app -> AuthPlugin app
authStellar config =
    AuthPlugin
        { apName     = pluginName
        , apLogin    = login
        , apDispatch = dispatch config
        }

type Method = Text

type Piece = Text

addressField :: Text
addressField = "stellar_address"

responseForm ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    AForm m Text
responseForm =
    unTextarea <$>
    areq
        textareaField
        (bfs ("3. Paste the signed piece here:" :: Text))
            {fsName = Just "response"}
        Nothing

data VerificationData = VerificationData
    { address   :: Stellar.Address
    , nonce     :: Text
    }

dispatch :: Config app -> Method -> [Piece] -> AuthHandler app TypedContent
dispatch config method _path =
    case method of
        "GET"   -> toTypedContent <$> makeChallengeForm config
        "POST"  -> receiveChallengeResponse config
        _       -> badMethod

makeChallengeForm :: Config app -> AuthHandler app Html
makeChallengeForm Config{getVerifyKey} = do
    routeToMaster <- getRouteToParent
    mAddress <- lookupGetParam addressField
    case mAddress of
        Nothing -> authLayout makeAddressForm
        Just address0 -> do
            let address = strip address0
            nonce <- liftHandler $ getVerifyKey $ Stellar.Address address
            challenge <- makeChallenge address nonce
            authLayout $ makeResponseForm routeToMaster challenge

receiveChallengeResponse :: Config app -> AuthHandler app TypedContent
receiveChallengeResponse config@Config{checkAndRemoveVerifyKey} = do
    ((result, _formWidget), _formEnctype) <- runFormPost responseForm
    case result of
        FormSuccess response -> do
            VerificationData{address, nonce} <- verifyResponse response
            verifyAccount config address
            ok <- liftHandler $ checkAndRemoveVerifyKey address nonce
            if ok then
                setCredsRedirect $ makeCreds address
            else
                invalidArgs ["Verification key is invalid or expired"]
        _ -> invalidArgs [Text.pack $ show result]

makeCreds :: Stellar.Address -> Creds app
makeCreds (Stellar.Address credsIdent) =
    Creds{credsPlugin = pluginName, credsIdent, credsExtra = []}

login :: (Route Auth -> Route app) -> WidgetFor app ()
login routeToMaster =
    [whamlet|
        <a href=@{start} role=button .btn.btn-primary>
            Create account or log in
    |]
  where
    start = routeToMaster pluginRoute

makeAddressForm :: RenderMessage app FormMessage => WidgetFor app ()
makeAddressForm = do
    (widget, enctype) <- generateFormGet addressForm
    [whamlet|
        <form method=get enctype=#{enctype}>
            ^{widget}
            <button type=submit .btn .btn-primary>Next
    |]

addressForm ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    AForm m (Maybe Text)
addressForm =
    aopt
        textField
        (bfs ("Stellar public address (starts with G):" :: Text))
            {fsName = Just "stellar_address"}
        Nothing

makeResponseForm ::
    RenderMessage app FormMessage =>
    (Route Auth -> Route app) -> Text -> WidgetFor app ()
makeResponseForm routeToMaster challenge = do
    (widget, enctype) <- generateFormPost responseForm
    [whamlet|
        $newline never
        <p>
            For the purpose of authentications in Veche,
            \ a special "request" transaction is made.
            \ Sign this transaction, but do not submit it, instead,
            \ paste the code below:
        <div .alert.alert-secondary>
            <tt .stellar_challenge #stellar_challenge
                    style="overflow-wrap: break-word;">
                #{challenge}
        <p>
            This is an almost empty, intentionally invalid transaction.
            \ It is costructed for the test network,
            \ has zero fee and zero sequence number,
            \ specifically to make sure it cannot be sent to the real network.
        <div .mb-3>
            <button .btn.btn-primary onclick="copy_tx()">
                <strong>1.
                \ Copy transaction
            \
            <a .btn.btn-secondary
                    href="https://laboratory.stellar.org/#xdr-viewer?input=#{challengeE}&type=TransactionEnvelope&network=public"
                    role=button target=_blank>
                View in Lab
            \
            <a .btn.btn-primary
                    href="https://laboratory.stellar.org/#txsigner?xdr=#{challengeE}"
                    role=button target=_blank>
                <strong>2.
                \ Sign in Lab
        <form method=post action=@{routeToMaster pluginRoute} enctype=#{enctype} id=auth_stellar_response_form>
            ^{widget}
            <button type=submit .btn .btn-primary .mt-3>
                <strong>4.
                \ Log in
    |]
    toWidget
        [julius|
            function copy_tx() {
                navigator.clipboard.writeText($('#stellar_challenge').text());
            }
        |]
  where
    challengeE = escapeURIString (not . isReserved) $ Text.unpack challenge

data InternalError = InternalErrorNonceTooLong
    deriving (Exception, Show)

makeChallenge :: MonadHandler m => Text -> Text -> m Text
makeChallenge address nonce0 = do
    publicKey <- decodePublicKey address ?| invalidArgs ["Bad address"]
    nonce <- lengthArray (encodeUtf8 nonce0) ?| internalErrorNonceTooLong
    pure $ makeChallenge' publicKey nonce
  where

    m ?| e = maybe e pure m

    internalErrorNonceTooLong = liftIO $ throwIO InternalErrorNonceTooLong

    makeChallenge' publicKey nonce =
        (transactionBuilder publicKey 0)
            { tbMemo = Just $ Memo'MEMO_TEXT loggingIntoVeche
            , tbOperations = [nonceOp nonce]
            }
        & buildWithFee 0
        & Stellar.toEnvelope
        & xdrSerialize
        & encodeBase64

    nonceOp value =
        Operation
            { operation'sourceAccount = Nothing
            , operation'body =
                OperationBody'MANAGE_DATA $ ManageDataOp "nonce" $ Just value
            }

loggingIntoVeche :: IsString s => s
loggingIntoVeche = "Logging into Veche"

verifyResponse :: MonadHandler m => Text -> m VerificationData
verifyResponse envelopeXdrBase64 = do
    envelope <- decodeEnvelope
    let Transaction{memo, operations, source} =
            Stellar.transactionFromEnvelopeXdr undefined $ traceShowId envelope
    verifySignatures envelope
    verifyMemo memo
    nonce <- getNonce operations
    pure VerificationData{address = source, nonce}
  where

    e ?| msg = either (const $ invalidArgs [msg]) pure e

    decodeEnvelope = do
        envelopeXdrRaw <-
            decodeBase64 (encodeUtf8 envelopeXdrBase64)
            ?| "Transaction envelope must be encoded as Base64"
        xdrDeserialize envelopeXdrRaw
            ?| "Transaction envelope must be encoded as XDR"

    verifySignatures envelope =
        unless (any (`verifySource` envelope) [publicNetwork, testNetwork]) $
            invalidArgs ["Signature is not verified"]

    verifyMemo memo =
        when (memo /= MemoText loggingIntoVeche) $ invalidArgs ["Bad memo"]

    getNonce = \case
        [OperationManageData "nonce" (Just nonce)] ->
            decodeUtf8' nonce ?| "Bad nonce"
        _ -> invalidArgs ["Bad operations"]

-- | Verify source address signature
verifySource :: Stellar.Network -> XDR.TransactionEnvelope -> Bool
verifySource net = \case
    TransactionEnvelope'ENVELOPE_TYPE_TX_V0
            (TransactionV0Envelope
                tx@TransactionV0{transactionV0'sourceAccountEd25519} signatures
            ) ->
        all (Stellar.verify
                net
                tx
                (Stellar.viewAccount $
                    PublicKey'PUBLIC_KEY_TYPE_ED25519
                        transactionV0'sourceAccountEd25519
                )
            )
            (unLengthArray signatures)
    TransactionEnvelope'ENVELOPE_TYPE_TX
            (TransactionV1Envelope
                tx@XDR.Transaction{transaction'sourceAccount} signatures
            ) ->
        all (Stellar.verify
                net
                tx
                (Stellar.viewAccount $
                    PublicKey'PUBLIC_KEY_TYPE_ED25519
                        case transaction'sourceAccount of
                            MuxedAccount'KEY_TYPE_ED25519         addr -> addr
                            MuxedAccount'KEY_TYPE_MUXED_ED25519 _ addr -> addr
                )
            )
            (unLengthArray signatures)
    TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP{} ->
        False -- this kind of transaction cannot be here

-- | Throws an exception on error
verifyAccount :: MonadHandler m => Config app -> Stellar.Address -> m ()
verifyAccount Config{horizon} address = do
    account <- getAccount'
    assert "account must be personal" $ isPersonal account
  where

    Stellar.Address rawAddress = address

    getAccount' = do
        eResult <-
            liftIO do
                manager <- newTlsManager
                runClientM (getAccount address) $ mkClientEnv manager horizon
        case eResult of
            Left (FailureResponse _ Response{responseStatusCode})
                | responseStatusCode == notFound404 ->
                    invalidArgs ["Account doesn't exist"]
            Left err -> liftIO $ throwIO err
            Right result -> pure result

    assert message condition
        | condition = pure ()
        | otherwise = do
            $logErrorS pluginName message
            notAuthenticated

    isPersonal Account{signers} =
        case signers of
            [Signer{key, weight}]   -> key == rawAddress && weight > 0
            _                       -> False

runFormPost ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    AForm m a -> m ((FormResult a, WidgetFor (HandlerSite m) ()), Yesod.Enctype)
runFormPost = Yesod.runFormPost . renderBootstrap5 BootstrapBasicForm

generateFormGet ::
    MonadHandler m =>
    AForm m a -> m (WidgetFor (HandlerSite m) (), Yesod.Enctype)
generateFormGet = Yesod.generateFormGet' . renderBootstrap5 BootstrapBasicForm

generateFormPost ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    AForm m a -> m (WidgetFor (HandlerSite m) (), Yesod.Enctype)
generateFormPost = Yesod.generateFormPost . renderBootstrap5 BootstrapBasicForm
