{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication.Telegram (authTelegram) where

-- prelude
import Foundation.Base
import Import.NoFoundation

-- global
import Yesod.Core (getYesod)
import Yesod.Form (runInputGet)

-- component
import Telegram.AuthWidget (AuthWidgetResponse (AuthWidgetResponse),
                            authWidgetResponse)
import Telegram.AuthWidget qualified

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
login routeToMaster = do
    App{appSettings = AppSettings{appTelegramBotName}} <- getYesod
    [whamlet|
        <script
            async
            data-auth-url=@{routeToMaster pluginRoute}
            data-radius=4
            data-request-access=write
            data-size=large
            data-telegram-login=#{appTelegramBotName}
            src="https://telegram.org/js/telegram-widget.js?19">
    |]

dispatch :: Method -> [Piece] -> AuthHandler App TypedContent
dispatch _method _path = do
    awr <- runInputGet authWidgetResponse
    setCredsRedirect $ makeCreds awr

makeCreds :: AuthWidgetResponse -> Creds app
makeCreds AuthWidgetResponse{id, username} =
    Creds
        { credsPlugin   = pluginName
        , credsIdent    = tshow id
        , credsExtra    = [("username", username)]
        }
