{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.User (getUserR, putUserR, userNameWidget) where

import Import

import Data.Char (isAscii, isPrint)
import Data.Text qualified as Text

getUserR :: Handler Html
getUserR = do
    (_, User{userName, userStellarAddress}) <- requireAuthPair
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

    if  | Text.null name    -> runDB $ update userId [UserName =. Nothing]
        | isValid name      -> runDB $ update userId [UserName =. Just name]
        | otherwise         ->
            sendResponseStatus status400 $
            object ["error" .= String "Name must be ASCII only"]

    returnJson Null

  where
    isValid = Text.all \c -> isAscii c && isPrint c

userNameWidget :: User -> Html
userNameWidget User{userName, userStellarAddress} =
    toHtml $
    case userName of
        Just name -> name <> " (" <> abbreviatedAddress <> ")"
        Nothing -> abbreviatedAddress
  where
    abbreviatedAddress = "*" <> Text.takeEnd 4 userStellarAddress
