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
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import Text.Shakespeare.Text (stextFile)
import Yesod.Auth (Auth, AuthPlugin (..), Route (PluginR))
import Yesod.Core (MonadHandler, TypedContent, WidgetFor, invalidArgs, liftIO,
                   lookupGetParam, newIdent, whamlet)

pluginName :: Text
pluginName = "stellar"

pluginRoute :: Route Auth
pluginRoute = PluginR pluginName []

authStellar :: AuthPlugin master
authStellar =
    AuthPlugin{apName = pluginName, apDispatch = dispatch, apLogin = login}

type Method = Text

type Piece = Text

addressField :: Text
addressField = "stellar_address"

-- dispatch :: Method -> [Piece] -> AuthHandler master TypedContent
dispatch :: Method -> [Piece] -> m TypedContent
dispatch _method _path = do
    -- mAddress <- lookupGetParam addressField
    -- routeToMaster <- getRouteToParent
    -- redirect
    --     ( routeToMaster LoginR
    --     , [(addressField, address) | Just address <- [mAddress]]
    --     )
    undefined

login :: (Route Auth -> Route master) -> WidgetFor master ()
login routeToMaster = do
    mAddress <- lookupGetParam addressField
    case mAddress of
        Nothing -> do
            ident <- newIdent
            [whamlet|
                <form method="get">
                    <label for="#{ident}">
                        Stellar public address (starts with G):
                    <input id="#{ident}" type="text" name="#{addressField}">
                    <input type="submit" value="Next">
            |]
        Just address -> do
            ident <- newIdent
            challenge <- makeChallenge address
            [whamlet|
                $newline never
                Sign this transaction:
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
                    <label for="#{ident}">And paste the signed piece here:
                    <div>
                        <textarea id="#{ident}" name="response">
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
