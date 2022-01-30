{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.Stellar (authStellar) where

import Data.Text (Text)
import Yesod.Auth (Auth, AuthHandler, AuthPlugin (..), Route (LoginR, PluginR))
import Yesod.Core (TypedContent, WidgetFor, getRouteToParent, lookupGetParam,
                   newIdent, redirect, whamlet)

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

dispatch :: Method -> [Piece] -> AuthHandler master TypedContent
dispatch _method _path = do
    mAddress <- lookupGetParam addressField
    routeToMaster <- getRouteToParent
    redirect
        ( routeToMaster LoginR
        , [(addressField, address) | Just address <- [mAddress]]
        )

login :: (Route Auth -> Route master) -> WidgetFor master ()
login routeToMaster = do
    mAddress <- lookupGetParam addressField
    case mAddress of
        Nothing -> do
            ident <- newIdent
            [whamlet|
                <form method="get" action="@{routeToMaster pluginRoute}">
                    <label for="#{ident}">
                        Stellar public address (starts with G):
                    <input id="#{ident}" type="text" name="#{addressField}">
                    <input type="submit" value="Next">
            |]
        Just address -> do
            ident <- newIdent
            [whamlet|
                <form method="get" action="@{routeToMaster pluginRoute}">
                    <label for="#{ident}">
                        Sign this transaction (for #{address}):
                    <input id="#{ident}" type="textarea" name="response">
                    <input type="submit" value="Log in">
            |]
