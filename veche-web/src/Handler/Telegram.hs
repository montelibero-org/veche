{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Telegram (getAuthTelegramR, postAuthTelegramUnlinkR) where

import Import

import Yesod.Form (intField, ireq, runInputGet)

import Model.User qualified as User

getAuthTelegramR :: Handler Void
getAuthTelegramR = do
    uid <- requireAuthId
    (id, username) <-
        runInputGet $ (,) <$> ireq intField "id" <*> ireq textField "username"
    User.setTelegram uid id username
    redirect UserR

postAuthTelegramUnlinkR :: Handler Void
postAuthTelegramUnlinkR = do
    uid <- requireAuthId
    User.deleteTelegram uid
    redirect UserR
