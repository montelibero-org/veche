{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.API (getApiCompleteUserR) where

import Import hiding (label)

import Data.Text qualified as Text

import Model.User qualified as User
import Templates.User (userNameText)

getApiCompleteUserR :: Handler Value
getApiCompleteUserR = do
    term <- lookupGetParam "term" ?|> invalidArgs ["`term` param must present"]
    users <- User.selectList
    pure $
        array
            [ object ["label" .= label, "value" .= userId]
            | Entity userId user@User{userStellarAddress} <- users
            , let label = userNameText user
            , Text.toLower term `isInfixOf` Text.toLower label
                || Text.toLower term `isInfixOf` Text.toLower userStellarAddress
            ]
