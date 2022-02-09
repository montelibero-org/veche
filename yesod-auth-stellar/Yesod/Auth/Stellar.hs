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
      mkAuthStellar
    ) where

import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..))
import Servant.Client (BaseUrl, ClientError (FailureResponse),
                       ResponseF (Response, responseStatusCode), mkClientEnv,
                       parseBaseUrl, runClientM)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import Text.Shakespeare.Text (stextFile)
import Yesod.Auth (Auth, AuthHandler, AuthPlugin (..), Creds (..),
                   Route (PluginR), setCredsRedirect)
import Yesod.Core (MonadHandler, TypedContent, WidgetFor, invalidArgs, liftIO,
                   logErrorS, lookupGetParam, lookupPostParam, newIdent,
                   notAuthenticated, whamlet)

import Stellar.Horizon.Client (getAccount, publicServerBase)
import Stellar.Horizon.Types (Account (..), Signer (..))

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
mkAuthStellar :: MonadThrow m => Maybe Text -> m (AuthPlugin app)
mkAuthStellar mBaseUrl = do
    baseUrl <-
        case mBaseUrl of
            Nothing  -> pure publicServerBase
            Just url -> parseBaseUrl $ Text.unpack url
    pure
        AuthPlugin
            { apName = pluginName
            , apLogin = login
            , apDispatch = dispatch baseUrl
            }

type Method = Text

type Piece = Text

addressField :: Text
addressField = "stellar_address"

responseField :: Text
responseField = "response"

dispatch :: BaseUrl -> Method -> [Piece] -> AuthHandler app TypedContent
dispatch baseUrl _method _path = do
    mResponse <- lookupPostParam responseField
    response <-
        case mResponse of
            Nothing -> invalidArgs [responseField <> " not found"]
            Just response -> pure response
    address <- verifyResponse response
    verifyAccount baseUrl address
    setCredsRedirect
        Creds{credsPlugin = pluginName, credsIdent = address, credsExtra = []}

login :: (Route Auth -> Route app) -> WidgetFor app ()
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
        <form method="get">
            <label for="#{ident}">
                Stellar public address (starts with G):
            <input id="#{ident}" type="text" name="#{addressField}">
            <input type="submit" value="Next">
    |]

challengeResponseForm :: (Route Auth -> Route app) -> Text -> WidgetFor app ()
challengeResponseForm routeToMaster challenge = do
    ident <- newIdent
    [whamlet|
        $newline never
        Sign this transaction, but do not submit it:
        <div>
            <code .stellar_challenge>#{challenge}
        <div>
            [
                <a href="https://laboratory.stellar.org/#xdr-viewer?input=#{challenge}&type=TransactionEnvelope" target="_blank">
                    View in Lab
            ] [
                <a href="https://laboratory.stellar.org/#txsigner?xdr=#{challenge}" target="_blank">
                    Sign in Lab
            ]
        <form method="post" action="@{routeToMaster pluginRoute}">
            <label for="#{ident}">Paste the signed piece here:
            <div>
                <textarea id="#{ident}" name="#{responseField}">
            <input type="submit" value="Log in">
    |]

makeChallenge :: MonadHandler m => Text -> m Text
makeChallenge address = do
    (exitCode, out, err) <-
        liftIO $
        readProcess $ proc "python3" [] & setStdin (byteStringInput program)
    case exitCode of
        ExitSuccess -> pure $ TextL.toStrict $ decodeUtf8 out
        ExitFailure _ -> invalidArgs [TextL.toStrict $ decodeUtf8 err]
  where
    program = encodeUtf8 $(stextFile "Yesod/Auth/challenge.py")

-- | Returns address
verifyResponse :: MonadHandler m => Text -> m Text
verifyResponse envelope = do
    (exitCode, out, err) <-
        liftIO $
        readProcess $ proc "python3" [] & setStdin (byteStringInput program)
    case exitCode of
        ExitSuccess -> pure $ Text.strip $ TextL.toStrict $ decodeUtf8 out
        ExitFailure _ -> invalidArgs [TextL.toStrict $ decodeUtf8 err]
  where
    program = encodeUtf8 $(stextFile "Yesod/Auth/verify.py")

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
