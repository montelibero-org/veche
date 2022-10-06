{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.User (
    userNameText,
    userNameWidget,
    userPage,
) where

import Import

import Data.Text qualified as Text
import Stellar.Horizon.Types qualified as Stellar
import Yesod (getsYesod)

import Model.Telegram (Telegram (Telegram))
import Model.Telegram qualified
import Model.User (User (User))
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

telegramWidget :: Telegram -> Widget
telegramWidget Telegram{username} = [whamlet|<samp>@#{username}|]

unlinkTeleram :: Widget
unlinkTeleram =
    actionButton
        AuthTelegramUnlinkR ["btn-danger"] "Unlink Telegram account" True

userPage :: Handler Html
userPage = do
    telegramBotName <- getsYesod $ appTelegramBotName . appSettings
    Entity uid user <- requireAuth
    let User{name, stellarAddress = Stellar.Address stellarAddress} = user
    isSigner <- User.isSigner user
    isHolder <- User.isHolder user
    mTelegram <- User.getTelegram uid
    defaultLayout do
        setTitle "Profile"
        $(widgetFile "user")
