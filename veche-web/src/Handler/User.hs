{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.User (getUserR, putUserR) where

import Import

import Data.Aeson (Value (Null, String))
import Data.Char (isAscii, isPrint)
import Data.Text qualified as Text
import Yesod (getsYesod)

import Model.User qualified as User

unlinkTelegramForm :: Form Void
unlinkTelegramForm =
    BForm
        { aform = submit "unlink" "Unlink Telegram account" ["btn-danger"]
        , action = Just AuthTelegramR
        , classes = ["form-inline", "unlink-telegram"]
        , footer = mempty
        }

getUserR :: Handler Html
getUserR = do
    telegramBotName <- getsYesod $ appTelegramBotName . appSettings
    Entity uid User{userName, userStellarAddress} <- requireAuth
    mTelegram <- User.getTelegram uid
    unlinkTelegram <- generateFormPostB unlinkTelegramForm
    defaultLayout do
        setTitle "Profile"
        $(widgetFile "user")

newtype UserEditRequest = UserEditRequest{name :: Text}
    deriving (FromJSON, Generic)

newtype UserEditResponse = UserEditResponse{editError :: Maybe Text}
    deriving (Generic, ToJSON)

putUserR :: Handler Value
putUserR = do
    -- input
    UserEditRequest{name = Text.strip -> name} <- requireCheckJsonBody
    userId <- requireAuthId

    if  | Text.null name    -> User.setName userId Nothing
        | isValid name      -> User.setName userId $ Just name
        | otherwise         ->
            sendResponseStatus status400 $
            object ["error" .= String "Name must be ASCII only"]

    returnJson Null

  where
    isValid = Text.all \c -> isAscii c && isPrint c
