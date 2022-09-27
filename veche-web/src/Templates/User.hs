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

import Model.User qualified as User

userNameWidget :: User -> Html
userNameWidget = toHtml . userNameText

userNameText :: User -> Text
userNameText User{userName, userStellarAddress = Stellar.Address address} =
    case userName of
        Just name -> name <> " (" <> abbreviatedAddress <> ")"
        Nothing -> abbreviatedAddress
  where
    abbreviatedAddress = "*" <> Text.takeEnd 4 address

telegramWidget :: Telegram -> Widget
telegramWidget Telegram{telegramUsername} =
    [whamlet|<samp>@#{telegramUsername}|]

userPage :: Handler Html
userPage = do
    telegramBotName <- getsYesod $ appTelegramBotName . appSettings
    Entity uid user <- requireAuth
    let User{userName, userStellarAddress = Stellar.Address stellarAddress} =
            user
    mTelegram <- User.getTelegram uid
    defaultLayout do
        setTitle "Profile"
        $(widgetFile "user")
