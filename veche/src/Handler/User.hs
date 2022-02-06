{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.User (getUserR, putUserR) where

import Import

getUserR :: Handler Html
getUserR = do
    (_, User{userName, userStellarAddress}) <- requireAuthPair
    defaultLayout do
        setTitle "Profile"
        $(widgetFile "user")

newtype UserEditRequest = UserEditRequest{name :: Text}
    deriving (FromJSON, Generic)

putUserR :: Handler Value
putUserR = do
    -- input
    UserEditRequest{name} <- requireCheckJsonBody
    userId <- requireAuthId

    runDB $ update userId [UserName =. Just name]
    returnJson Null
