{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Templates.User (
    userNameText,
    userNameWidget,
    userPage,
) where

-- prelude
import Foundation.Base
import Import.NoFoundation

-- global
import Data.Text qualified as Text
import Stellar.Horizon.Client qualified as Stellar
import Yesod (getsYesod)

-- component
import Model.Telegram (Telegram (Telegram))
import Model.Telegram qualified
import Model.User (User (User), UserId)
import Model.User qualified as User

userNameWidget :: User -> Html
userNameWidget = toHtml . userNameText

userNameText :: User -> Text
userNameText User{name = mName, stellarAddress = Stellar.Address address} =
    case mName of
        Just name   -> name <> " (" <> abbreviatedAddress <> ")"
        Nothing     -> abbreviatedAddress
  where
    abbreviatedAddress = "*" <> Text.takeEnd 4 address

telegramUsernameWidget :: Telegram -> Widget
telegramUsernameWidget Telegram{username} = [whamlet|<samp>@#{username}|]

unbindTeleram :: Widget
unbindTeleram =
    actionButton TelegramUnbindR ["btn-danger"] "Unbind Telegram account" True

userPage ::
    (AuthEntity App ~ User, AuthId App ~ UserId, YesodAuthPersist App) =>
    Handler Html
userPage = do
    telegramBotName <- getsYesod $ appTelegramBotName . appSettings
    Entity uid user <- requireAuth
    (_uid, roles) <- User.requireAuthzRoles
    let User{name, stellarAddress = Stellar.Address stellarAddress} = user
    mTelegram <- User.getTelegram uid
    defaultLayout do
        setTitle "Profile"
        $(widgetFile "user")
