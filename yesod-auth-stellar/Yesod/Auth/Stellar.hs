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
    ) where

import Control.Exception (throwIO)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Servant.Client (BaseUrl, ClientError (FailureResponse),
                       ResponseF (Response, responseStatusCode), mkClientEnv,
                       runClientM)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import Text.Shakespeare.Text (stextFile)
import Yesod.Auth (Auth, AuthHandler, AuthPlugin (..), Creds (..),
                   Route (PluginR), setCredsRedirect)
import Yesod.Core (HandlerSite, MonadHandler, RenderMessage, TypedContent,
                   WidgetFor, invalidArgs, liftIO, logErrorS, lookupGetParam,
                   newIdent, notAuthenticated, whamlet)
import Yesod.Form (AForm, FormMessage, FormResult (FormSuccess), areq, fsName,
                   generateFormPost, runFormPost, textareaField, unTextarea)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

import Stellar.Horizon.Client (getAccount)
import Stellar.Horizon.Types (Account (..), Signer (..))

type TextL = TextL.Text

pluginName :: Text
pluginName = "stellar"

pluginRoute :: Route Auth
pluginRoute = PluginR pluginName []

-- | Flow:
-- 1. 'login' shows 'addressForm'.
-- 2. User enters address (public key).
-- 3. 'login' shows 'challengeResponseForm' with challenge (dummy transaction)
--    based on address.
-- 4. User signs the transaction and enters signed envelope to the form.
-- 5. 'dispatch' verifies the signature and assigns credentials.
authStellar :: RenderMessage app FormMessage => BaseUrl -> AuthPlugin app
authStellar baseUrl =
    AuthPlugin
        {apName = pluginName, apLogin = login, apDispatch = dispatch baseUrl}

type Method = Text

type Piece = Text

-- TODO runFormGet
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

dispatch :: BaseUrl -> Method -> [Piece] -> AuthHandler app TypedContent
dispatch baseUrl _method _path = do
    ((result, _formWidget), _formEnctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm responseForm
    case result of
        FormSuccess response -> do
            address <- verifyResponse response
            verifyAccount baseUrl address
            setCredsRedirect
                Creds
                    { credsPlugin   = pluginName
                    , credsIdent    = address
                    , credsExtra    = []
                    }
        _ -> invalidArgs [Text.pack $ show result]

login ::
    RenderMessage app FormMessage =>
    (Route Auth -> Route app) -> WidgetFor app ()
login routeToMaster = do
    mAddress <- lookupGetParam addressField
    case mAddress of
        Nothing -> addressForm
        Just address -> do
            challenge <- makeChallenge address
            challengeResponseForm routeToMaster challenge

addressForm :: WidgetFor app ()
addressForm = do
    ident <- newIdent
    [whamlet|
        <form method=get>
            <label for=#{ident}>Stellar public address (starts with G):
            <input id=#{ident} type=text name=#{addressField}>
            <button type=submit>Next
    |]

challengeResponseForm ::
    RenderMessage app FormMessage =>
    (Route Auth -> Route app) -> Text -> WidgetFor app ()
challengeResponseForm routeToMaster challenge = do
    (formWidget, formEnctype) <-
        generateFormPost $ renderBootstrap3 BootstrapBasicForm responseForm
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
        <form method=post action=@{routeToMaster pluginRoute} enctype=#{formEnctype} id=auth_stellar_response_form>
            ^{formWidget}
            <button type=submit .btn .btn-primary>Log in
    |]

makeChallenge :: MonadHandler m => Text -> m Text
makeChallenge address = python3 $(stextFile "Yesod/Auth/challenge.py")

-- | Returns address
verifyResponse :: MonadHandler m => Text -> m Text
verifyResponse envelope = python3 $(stextFile "Yesod/Auth/verify.py")

python3 :: MonadHandler m => TextL -> m Text
python3 program = do
    (exitCode, out, err) <-
        liftIO $
        readProcess $
        proc "python3" [] & setStdin (byteStringInput $ encodeUtf8 program)
    case exitCode of
        ExitSuccess -> pure $ Text.strip $ TextL.toStrict $ decodeUtf8 out
        ExitFailure _ -> invalidArgs [TextL.toStrict $ decodeUtf8 err]

-- | Throws an exception on error
verifyAccount :: MonadHandler m => BaseUrl -> Text -> m ()
verifyAccount baseUrl address = do
    account <- getAccount'
    assert "account must be personal" $ isPersonal account
  where

    getAccount' = do
        eResult <-
            liftIO do
                manager <- newTlsManager
                runClientM (getAccount address) $ mkClientEnv manager baseUrl
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
