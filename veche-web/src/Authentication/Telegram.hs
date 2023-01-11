{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication.Telegram (authTelegram, pluginName, pluginRoute) where

-- prelude
import Foundation.Base
import Import.NoFoundation hiding (hash)

-- component
import Telegram.Auth (AuthorizationData (AuthorizationData),
                      receiveAuthorizationData)
import Telegram.Auth qualified

type Method = Text

type Piece = Text

pluginName :: Text
pluginName = "telegram"

pluginRoute :: Route Auth
pluginRoute = PluginR pluginName []

authTelegram :: AuthPlugin App
authTelegram =
    AuthPlugin
        { apName        = pluginName
        , apLogin       = login
        , apDispatch    = dispatch
        }

login :: (Route Auth -> Route App) -> Widget
login _routeToMaster =
    [whamlet|
        <a .btn.btn-primary
                href="http://t.me/mtl_veche_bot?start=login" role=button>
            @mtl_veche_bot
    |]
    -- [whamlet|
    --     <script
    --         async
    --         data-auth-url=@{routeToMaster pluginRoute}
    --         data-radius=4
    --         data-request-access=write
    --         data-size=large
    --         data-telegram-login=#{appTelegramBotName}
    --         src="https://telegram.org/js/telegram-widget.js?19">
    -- |]

dispatch :: Method -> [Piece] -> AuthHandler App TypedContent
dispatch _method _path = do
    ad <- receiveAuthorizationData
    setCredsRedirect $ makeCreds ad

makeCreds :: AuthorizationData -> Creds app
makeCreds AuthorizationData{id, username} =
    Creds
        { credsPlugin   = pluginName
        , credsIdent    = tshow id
        , credsExtra    = [("username", u) | u <- toList username]
        }
