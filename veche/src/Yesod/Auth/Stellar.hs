{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.Stellar (authStellar) where

import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import Text.Shakespeare.Text (stextFile)
import Yesod.Auth (Auth, AuthHandler, AuthPlugin (..), Creds (..),
                   Route (PluginR), setCredsRedirect)
import Yesod.Core (MonadHandler, TypedContent, WidgetFor, invalidArgs, liftIO,
                   lookupGetParam, lookupPostParam, newIdent, whamlet)

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
-- 4. 'dispatch' verifies the signature and assigns credentials.
authStellar :: AuthPlugin app
authStellar =
    AuthPlugin{apName = pluginName, apDispatch = dispatch, apLogin = login}

type Method = Text

type Piece = Text

addressField :: Text
addressField = "stellar_address"

responseField :: Text
responseField = "response"

dispatch :: Method -> [Piece] -> AuthHandler app TypedContent
dispatch _method _path = do
    mResponse <- lookupPostParam responseField
    response <-
        case mResponse of
            Nothing -> invalidArgs [responseField <> " not found"]
            Just response -> pure response
    address <- verifyResponse response
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
    program = encodeUtf8 $(stextFile "src/Yesod/Auth/challenge.py")

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
    program = encodeUtf8 $(stextFile "src/Yesod/Auth/verify.py")
