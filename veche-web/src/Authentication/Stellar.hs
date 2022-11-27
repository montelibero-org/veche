{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication.Stellar (
    -- * Authentication plugin
    authnStellar,
    Flavor (..),
    Config (..),
) where

-- prelude
import Foundation.Base
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
import Yesod.Core (HandlerSite, badMethod, getRouteToParent, liftHandler,
                   logErrorS, notAuthenticated, toTypedContent)
import Yesod.Form (runInputGet)
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

data Flavor = Keybase | Laboratory
    deriving (Read, Show)

instance PathPiece Flavor where
    toPathPiece     = tshow
    fromPathPiece   = readMaybe . Text.unpack

data Config =
    Config
    { flavor                  :: Flavor
    , horizon                 :: BaseUrl
    , getVerifyKey            :: Stellar.Address -> Handler Text
    , checkAndRemoveVerifyKey :: Stellar.Address -> Text -> Handler Bool
    }

authnStellar :: Config -> AuthPlugin App
authnStellar config@Config{flavor} =
    AuthPlugin
    { apName     = pluginName
    , apLogin    = login flavor
    , apDispatch = dispatch config
    }

type Method = Text

type Piece = Text

addressField :: Text
addressField = "stellar_address"

responseForm ::
    (HandlerSite m ~ App, MonadHandler m) => Route App -> BForm m Text
responseForm action =
    (bform $
        unTextarea
        <$> areq
                textareaField
                (bfs MsgAuthnStellar3PasteSigned){fsName = Just "response"}
                Nothing
    )
    { action = Just action
    , footer =
        [whamlet|
            <button type=submit .btn .btn-primary .mt-3>
                <strong>4.
                \ _{MsgLogIn}
        |]
    , id = Just "auth_stellar_response_form"
    }

data VerificationData = VerificationData
    { address   :: Stellar.Address
    , nonce     :: Text
    }

dispatch :: Config -> Method -> [Piece] -> AuthHandler App TypedContent
dispatch config method _path =
    case method of
        "GET"   -> toTypedContent <$> makeChallengeForm config
        "POST"  -> receiveChallengeResponse config
        _       -> badMethod

makeChallengeForm :: Config -> AuthHandler App Html
makeChallengeForm Config{getVerifyKey} = do
    routeToMaster <- getRouteToParent
    flavorT <- runInputGet $ ireq textField "flavor"
    flavor <- fromPathPiece flavorT ?| invalidArgs ["Bad flavor"]
    mAddress <- lookupGetParam addressField
    case mAddress of
        Nothing -> authLayout $ makeAddressForm flavor
        Just address0 -> do
            let address = Text.strip address0
            nonce <- liftHandler $ getVerifyKey $ Stellar.Address address
            challenge <- makeChallenge address nonce
            authLayout $ makeResponseForm flavor routeToMaster challenge

receiveChallengeResponse :: Config -> AuthHandler App TypedContent
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

makeCreds :: Stellar.Address -> Creds App
makeCreds (Stellar.Address credsIdent) =
    Creds{credsPlugin = pluginName, credsIdent, credsExtra = []}

login :: Flavor -> (Route Auth -> Route App) -> Widget
login flavor routeToMaster =
    [whamlet|
        <a href=@?{start} role=button .btn.btn-primary>
            _{msgLogInVia}
    |]
  where

    start = (routeToMaster pluginRoute, [("flavor", toPathPiece flavor)])

    msgLogInVia =
        case flavor of
            Keybase     -> MsgLogInViaKeybase
            Laboratory  -> MsgLogInViaStellarLaboratory

makeAddressForm :: Flavor -> Widget
makeAddressForm flavor =
    join $
    generateFormGetB
        (bform $
            areq hiddenField ""{fsName = Just "flavor"} (Just flavor)
            *>
            areq
                textField
                (bfs MsgLabelStellarPublicAddress){fsName = Just addressField}
                Nothing
        )
        {footer = [whamlet|<button type=submit .btn .btn-primary>Next|]}

makeResponseForm :: Flavor -> (Route Auth -> Route App) -> Text -> Widget
makeResponseForm flavor routeToMaster challenge = do
    widget <- generateFormPostB $ responseForm $ routeToMaster pluginRoute
    case flavor of
        Keybase     -> $(widgetFile "auth/stellar-keybase")
        Laboratory  -> $(widgetFile "auth/stellar-laboratory")
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

(?) :: MonadHandler m => Either e a -> Text -> m a
e ? msg = either (const $ invalidArgs [msg]) pure e

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
verifyAccount :: MonadHandler m => Config -> Stellar.Address -> m ()
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
