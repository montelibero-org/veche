{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication.Stellar
    (
    -- * Authentication plugin
      authnStellar
    , Config (..)
    ) where

-- prelude
import Import.NoFoundation hiding (assert)

-- global
import Data.ByteString.Base64 qualified as Base64
import Data.Text qualified as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (notFound404, urlEncode)
import Network.ONCRPC.XDR (lengthArray, xdrDeserialize, xdrSerialize)
import Network.Stellar.Builder (buildWithFee, tbMemo, tbOperations,
                                transactionBuilder)
import Network.Stellar.Builder qualified as Stellar
import Network.Stellar.Keypair (decodePublicKey)
import Network.Stellar.Network (publicNetwork, testNetwork)
import Network.Stellar.TransactionXdr (ManageDataOp (ManageDataOp),
                                       Memo (Memo'MEMO_TEXT),
                                       Operation (Operation),
                                       OperationBody (OperationBody'MANAGE_DATA))
import Network.Stellar.TransactionXdr qualified as XDR
import Servant.Client (BaseUrl, ClientError (FailureResponse),
                       ResponseF (Response, responseStatusCode), mkClientEnv,
                       runClientM)
import Yesod.Core (HandlerSite, RenderMessage, WidgetFor, badMethod,
                   getRouteToParent, liftHandler, logErrorS, notAuthenticated,
                   toTypedContent)
import Yesod.Form (FormMessage)
import Yesod.Form.Bootstrap5 (bfs)

-- project
import Stellar.Horizon.Client (Account (Account),
                               Operation (OperationManageData), Signer (Signer),
                               Transaction (Transaction), getAccount)
import Stellar.Horizon.Client qualified as Stellar
import Stellar.Simple (Memo (MemoText), verifyTx)

pluginName :: Text
pluginName = "stellar"

pluginRoute :: Route Auth
pluginRoute = PluginR pluginName []

data Config app = Config
    { horizon :: BaseUrl
    , getVerifyKey :: Stellar.Address -> HandlerFor app Text
    , checkAndRemoveVerifyKey :: Stellar.Address -> Text -> HandlerFor app Bool
    }

authnStellar :: Config app -> AuthPlugin app
authnStellar config =
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
    Route site -> BForm m Text
responseForm action =
    (bform $
        unTextarea
        <$> areq
                textareaField
                (bfs ("3. Paste the signed piece here:" :: Text))
                    {fsName = Just "response"}
                Nothing)
    { action = Just action
    , footer =
        [whamlet|
            <button type=submit .btn .btn-primary .mt-3>
                <strong>4.
                \ Log in
        |]
    , id = Just "auth_stellar_response_form"
    }

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
            let address = Text.strip address0
            nonce <- liftHandler $ getVerifyKey $ Stellar.Address address
            challenge <- makeChallenge address nonce
            authLayout $ makeResponseForm routeToMaster challenge

receiveChallengeResponse :: Config app -> AuthHandler app TypedContent
receiveChallengeResponse config@Config{checkAndRemoveVerifyKey} = do
    routeToMaster <- getRouteToParent
    (result, _formWidget) <-
        runFormPostB $ responseForm $ routeToMaster pluginRoute
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
            Stellar Laboratory
    |]
  where
    start = routeToMaster pluginRoute

makeAddressForm :: RenderMessage app FormMessage => WidgetFor app ()
makeAddressForm = join $ generateFormGetB addressForm

addressForm ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    BForm m (Maybe Text)
addressForm =
    (bform $
        aopt
            textField
            (bfs ("Stellar public address (starts with G):" :: Text))
                {fsName = Just "stellar_address"}
            Nothing
    )
    {footer = [whamlet|<button type=submit .btn .btn-primary>Next|]}

makeResponseForm ::
    RenderMessage app FormMessage =>
    (Route Auth -> Route app) -> Text -> WidgetFor app ()
makeResponseForm routeToMaster challenge = do
    widget <- generateFormPostB $ responseForm $ routeToMaster pluginRoute
    -- let challengeHref =
    --         mconcat
    --             [ "web+stellar:tx?xdr=", toQueryParam challenge
    --             , "&callback="
    --             , toQueryParam $ renderUrl $ routeToMaster pluginRoute
    --             ]
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
        ^{widget}
    |]
    toWidget
        [julius|
            function copy_tx() {
                navigator.clipboard.writeText($('#stellar_challenge').text());
            }
        |]
  where
    challengeE = decodeUtf8Throw $ urlEncode False $ encodeUtf8 challenge

data InternalError = InternalErrorNonceTooLong
    deriving (Exception, Show)

makeChallenge :: MonadHandler m => Text -> Text -> m Text
makeChallenge address nonce0 = do
    publicKey <- decodePublicKey address ?| invalidArgs ["Bad address"]
    nonce <- lengthArray (encodeUtf8 nonce0) ?| internalErrorNonceTooLong
    pure $ makeChallenge' publicKey nonce
  where

    internalErrorNonceTooLong = liftIO $ throwIO InternalErrorNonceTooLong

    makeChallenge' publicKey nonce =
        (transactionBuilder publicKey 0)
            { tbMemo = Just $ Memo'MEMO_TEXT loggingIntoVeche
            , tbOperations = [nonceOp nonce]
            }
        & buildWithFee 0
        & Stellar.toEnvelope
        & xdrSerialize
        & Base64.encode
        & decodeUtf8Throw

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
    let Transaction{memo, operations, source, signatures} =
            Stellar.transactionFromXdrEnvelope envelope
    verifySignatures source signatures envelope
    verifyMemo memo
    nonce <- getNonce operations
    pure VerificationData{address = source, nonce}
  where

    e ? msg = either (const $ invalidArgs [msg]) pure e

    decodeEnvelope = do
        envelopeXdrRaw <-
            Base64.decode (encodeUtf8 envelopeXdrBase64)
            ? "Transaction envelope must be encoded as Base64"
        xdrDeserialize envelopeXdrRaw
            ? "Transaction envelope must be encoded as XDR"

    verifySignatures source signatures envelope =
        unless
            (or [ verifyTx net envelope source signature
                | signature <- signatures
                , net <- [publicNetwork, testNetwork]
                ]
            )
            (invalidArgs ["Signature is not verified"])

    verifyMemo memo =
        when (memo /= MemoText loggingIntoVeche) $ invalidArgs ["Bad memo"]

    getNonce = \case
        [Right (OperationManageData "nonce" (Just nonce))] -> pure nonce
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
