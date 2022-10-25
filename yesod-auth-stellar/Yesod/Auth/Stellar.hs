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

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Text (Text, strip)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (notFound404)
import Network.ONCRPC.XDR (emptyBoundedLengthArray, lengthArray, unLengthArray,
                           xdrDeserialize, xdrSerialize)
import Network.Stellar.Builder (buildWithFee, tbMemo, tbOperations,
                                transactionBuilder, verify, viewAccount)
import Network.Stellar.Keypair (decodePublicKey, encodePublicKey)
import Network.Stellar.Network (publicNetwork, testNetwork)
import Network.Stellar.TransactionXdr (DataValue, ManageDataOp (ManageDataOp),
                                       Memo (Memo'MEMO_TEXT),
                                       Operation (Operation),
                                       OperationBody (OperationBody'MANAGE_DATA),
                                       Transaction (Transaction),
                                       TransactionEnvelope (TransactionEnvelope))
import Network.Stellar.TransactionXdr qualified
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
import Stellar.Horizon.Client (getAccount)
import Stellar.Horizon.Types (Account (Account), Signer (Signer))
import Stellar.Horizon.Types qualified as Stellar

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
        <a href=@{start} role=button .btn.btn-warning>
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
        <div .panel.panel-default style="background: #eee;">
            <div .panel-body>
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
            <a .btn.btn-outline-primary
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
            {tbMemo = Just loggingIntoVeche, tbOperations = [NonceOp nonce]}
        & buildWithFee 0
        & toEnvelope
        & xdrSerialize
        & encodeBase64

    toEnvelope tx = TransactionEnvelope tx emptyBoundedLengthArray

pattern NonceOp :: DataValue -> Operation
pattern NonceOp value =
    Operation
    { operation'sourceAccount = Nothing
    , operation'body =
        OperationBody'MANAGE_DATA (ManageDataOp "nonce" (Just value))
    }

loggingIntoVeche :: Memo
loggingIntoVeche = Memo'MEMO_TEXT "Logging into Veche"

verifyResponse :: MonadHandler m => Text -> m VerificationData
verifyResponse envelopeXdrBase64 = do
    envelope <- decodeEnvelope
    let TransactionEnvelope{transactionEnvelope'tx} = envelope
        Transaction
                { transaction'memo
                , transaction'operations
                , transaction'sourceAccount
                } =
            transactionEnvelope'tx
        stellarAddress = viewAccount transaction'sourceAccount
        horizonAddress = Stellar.Address $ encodePublicKey stellarAddress
    verifySignature stellarAddress envelope
    verifyMemo transaction'memo
    nonce <- getNonce transaction'operations
    pure VerificationData{address = horizonAddress, nonce}
  where

    e ?| msg = either (const $ invalidArgs [msg]) pure e

    decodeEnvelope = do
        envelopeXdrRaw <-
            decodeBase64 (encodeUtf8 envelopeXdrBase64)
            ?| "Transaction envelope must be encoded as Base64"
        xdrDeserialize envelopeXdrRaw
            ?| "Transaction envelope must be encoded as XDR"

    verifySignature account (TransactionEnvelope tx signatures) = do
        signature <-
            case toList $ unLengthArray signatures of
                [signature] -> pure signature
                _ -> invalidArgs ["Expected exactly 1 signature"]
        let verified =
                or  [ verify network tx account signature
                    | network <- [publicNetwork, testNetwork]
                    ]
        unless verified $ invalidArgs ["Signature is not verified"]

    verifyMemo transaction'memo =
        unless (transaction'memo == loggingIntoVeche) $
            invalidArgs ["Bad memo"]

    getNonce transaction'operations =
        case toList $ unLengthArray transaction'operations of
            [NonceOp nonce] -> decodeUtf8' (unLengthArray nonce) ?| "Bad nonce"
            _ -> invalidArgs ["Bad operations"]

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
