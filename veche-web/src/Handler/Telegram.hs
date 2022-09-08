{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Telegram (getAuthTelegramR, postAuthTelegramR) where

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

postAuthTelegramR :: Handler Void
postAuthTelegramR = do
    uid <- requireAuthId
    result <- getPostAction
    case result of
        FormSuccess "unlink" -> do
            User.deleteTelegram uid
            redirect UserR
        _ -> invalidArgs [tshow result]
