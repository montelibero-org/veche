{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.API (getApiCompleteUserR) where

import Import

import Model.User qualified as User
import Templates.User (userNameText)

getApiCompleteUserR :: Handler Value
getApiCompleteUserR = do
    users <- User.selectList
    pure $
        array
            [ object ["label" .= userNameText user, "value" .= userId]
            | Entity userId user <- users
            ]
