{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Crypto.Nonce (nonce128urlT)
import Crypto.Nonce qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Servant.Client (BaseUrl, ClientError (FailureResponse),
                       ResponseF (Response, responseStatusCode), mkClientEnv,
                       runClientM)
import System.IO.Unsafe (unsafePerformIO)
import Text.Shakespeare.Text (stextFile)
import Yesod.Auth (Auth, AuthHandler, AuthPlugin (..), Creds (..),
                   Route (PluginR), setCredsRedirect)
import Yesod.Core (HandlerFor, HandlerSite, MonadHandler, RenderMessage,
                   TypedContent, WidgetFor, invalidArgs, liftHandler, liftIO,
                   logErrorS, lookupGetParam, notAuthenticated, whamlet)
import Yesod.Form (AForm, FormMessage, FormResult (FormSuccess), aopt, areq,
                   fsName, textField, textareaField, unTextarea)
import Yesod.Form qualified as Yesod
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

-- project
import Stellar.Horizon.Client (getAccount)
import Stellar.Horizon.Types (Account (..), Signer (..))

-- component
import Yesod.Auth.Stellar.Internal (TextL, python3)

pluginName :: Text
pluginName = "stellar"

pluginRoute :: Route Auth
pluginRoute = PluginR pluginName []

data Config app = Config
    { horizon :: BaseUrl
    , setVerifyKey :: Text -> Text -> WidgetFor app ()
    , checkAndRemoveVerifyKey :: Text -> Text -> HandlerFor app Bool
    }

-- | Flow:
-- 1. 'login' shows 'addressForm'.
-- 2. User enters address (public key).
-- 3. 'login' shows 'responseForm' with challenge (dummy transaction)
--    based on address.
-- 4. User signs the transaction and enters signed envelope to the form.
-- 5. 'dispatch' verifies the signature and assigns credentials.
authStellar :: RenderMessage app FormMessage => Config app -> AuthPlugin app
authStellar config =
    AuthPlugin
        { apName     = pluginName
        , apLogin    = login config
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
        (bfs ("Paste the signed piece here:" :: Text)){fsName = Just "response"}
        Nothing

dispatch :: Config app -> Method -> [Piece] -> AuthHandler app TypedContent
dispatch config@Config{checkAndRemoveVerifyKey} _method _path = do
    ((result, _formWidget), _formEnctype) <- runFormPost responseForm
    case result of
        FormSuccess response -> do
            (address, nonce) <- verifyResponse response
            verifyAccount config address
            ok <- liftHandler $ checkAndRemoveVerifyKey address nonce
            if ok then
                setCredsRedirect
                    Creds
                        { credsPlugin = pluginName
                        , credsIdent  = address
                        , credsExtra  = []
                        }
            else
                invalidArgs ["Verification key is invalid or expired"]
        _ -> invalidArgs [Text.pack $ show result]

login ::
    RenderMessage app FormMessage =>
    Config app -> (Route Auth -> Route app) -> WidgetFor app ()
login Config{setVerifyKey} routeToMaster = do
    mAddress <- lookupGetParam addressField
    case mAddress of
        Nothing -> makeAddressForm
        Just address -> do
            nonce <- nonce128urlT nonceGenerator
            setVerifyKey address nonce
            challenge <- makeChallenge address nonce
            makeResponseForm routeToMaster challenge

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
        Sign this transaction, but do not submit it:
        <div>
            <code .stellar_challenge style="overflow-wrap: break-word;">
                #{challenge}
        <div>
            [
                <a href="https://laboratory.stellar.org/#xdr-viewer?input=#{challenge}&type=TransactionEnvelope" target="_blank">
                    View in Lab
            ] [
                <a href="https://laboratory.stellar.org/#txsigner?xdr=#{challenge}" target="_blank">
                    Sign in Lab
            ]
        <form method=post action=@{routeToMaster pluginRoute} enctype=#{enctype} id=auth_stellar_response_form>
            ^{widget}
            <button type=submit .btn .btn-primary>Log in
    |]

makeChallenge :: MonadHandler m => Text -> Text -> m Text
makeChallenge address nonce = python3' $(stextFile "Yesod/Auth/challenge.py")

-- | Returns address and nonce
verifyResponse :: MonadHandler m => Text -> m (Text, Text)
verifyResponse envelope = do
    addressNonce <- python3' $(stextFile "Yesod/Auth/verify.py")
    case Text.words addressNonce of
        [address, nonce] -> pure (address, nonce)
        _ -> invalidArgs ["Bad transaction"]

python3' :: MonadHandler m => TextL -> m Text
python3' = python3 >=> either (\msg -> invalidArgs [msg]) pure

-- | Throws an exception on error
verifyAccount :: MonadHandler m => Config app -> Text -> m ()
verifyAccount Config{horizon} address = do
    account <- getAccount'
    assert "account must be personal" $ isPersonal account
  where

    getAccount' = do
        eResult <-
            liftIO do
                manager <- newTlsManager
                runClientM (getAccount address) $ mkClientEnv manager horizon
        case eResult of
            Left (FailureResponse _ Response{responseStatusCode})
                | Status{statusCode = 404} <- responseStatusCode ->
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
            [Signer{key, weight}]   -> key == address && weight > 0
            _                       -> False

runFormPost ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    AForm m a -> m ((FormResult a, WidgetFor (HandlerSite m) ()), Yesod.Enctype)
runFormPost = Yesod.runFormPost . renderBootstrap3 BootstrapBasicForm

generateFormGet ::
    MonadHandler m =>
    AForm m a -> m (WidgetFor (HandlerSite m) (), Yesod.Enctype)
generateFormGet = Yesod.generateFormGet' . renderBootstrap3 BootstrapBasicForm

generateFormPost ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    AForm m a -> m (WidgetFor (HandlerSite m) (), Yesod.Enctype)
generateFormPost = Yesod.generateFormPost . renderBootstrap3 BootstrapBasicForm

nonceGenerator :: Crypto.Nonce.Generator
nonceGenerator = unsafePerformIO Crypto.Nonce.new
{-# NOINLINE nonceGenerator #-}
