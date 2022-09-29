{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.User (getUserR, putUserR) where

import Import

import Data.Aeson (Value (Null, String))
import Data.Char (isAscii, isPrint)
import Data.Text qualified as Text

import Model.User qualified as User
import Templates.User (userPage)

getUserR :: Handler Html
getUserR = userPage

newtype UserEditRequest = UserEditRequest{name :: Text}
    deriving (FromJSON, Generic)

newtype UserEditResponse = UserEditResponse{editError :: Maybe Text}
    deriving (Generic, ToJSON)

putUserR :: Handler Value
putUserR = do
    -- input
    UserEditRequest{name = nameInput} <- requireCheckJsonBody
    let nameFixed = Text.strip nameInput
    userId <- requireAuthId

    if  | Text.null nameFixed -> User.setName userId Nothing
        | isValid nameFixed   -> User.setName userId $ Just nameFixed
        | otherwise           ->
            sendResponseStatus status400 $
            object ["error" .= String "Name must be ASCII only"]

    returnJson Null

  where
    isValid = Text.all \c -> isAscii c && isPrint c
