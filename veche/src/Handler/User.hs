{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.User (getUserR, putUserR) where

import Import

import Data.Char (isAscii, isPrint)
import Data.Text qualified as Text

import Model.User qualified as User

getUserR :: Handler Html
getUserR = do
    Entity _ User{userName, userStellarAddress} <- requireAuth
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
