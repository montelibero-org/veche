{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Telegram (getTelegramBindR, postTelegramUnbindR) where

-- prelude
import Import

-- global
import Yesod.Form (runInputGet)

-- component
import Model.User qualified as User
import Telegram.AuthWidget (AuthWidgetResponse (AuthWidgetResponse),
                            authWidgetResponse)
import Telegram.AuthWidget qualified

getTelegramBindR :: Handler Void
getTelegramBindR = do
    uid <- requireAuthId
    AuthWidgetResponse{id, username} <- runInputGet authWidgetResponse
    User.setTelegram uid id username
    redirect UserR

postTelegramUnbindR :: Handler Void
postTelegramUnbindR = do
    uid <- requireAuthId
    User.deleteTelegram uid
    redirect UserR
