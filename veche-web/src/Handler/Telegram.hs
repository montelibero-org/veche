{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Telegram (getTelegramBindR, postTelegramUnbindR) where

-- prelude
import Import

-- component
import Model.User qualified as User
import Telegram.Auth (AuthorizationData (AuthorizationData),
                      receiveAuthorizationData)
import Telegram.Auth qualified

getTelegramBindR :: Handler Void
getTelegramBindR = do
    uid <- requireAuthId
    AuthorizationData{id, username} <- receiveAuthorizationData
    User.setTelegram uid id username
    redirect UserR

postTelegramUnbindR :: Handler Void
postTelegramUnbindR = do
    uid <- requireAuthId
    User.deleteTelegram uid
    redirect UserR
