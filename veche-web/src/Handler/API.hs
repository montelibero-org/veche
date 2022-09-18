{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.API (getApiCompleteUserR) where

import Import hiding (label)

import Data.Text qualified as Text
import Stellar.Horizon.Types qualified as Stellar

import Model.User qualified as User
import Templates.User (userNameText)

getApiCompleteUserR :: Handler Value
getApiCompleteUserR = do
    term <- lookupGetParam "term" ?|> invalidArgs ["`term` param must present"]
    users <- User.selectAll
    pure $
        array
            [ object ["label" .= label, "value" .= userId]
            | Entity userId user <- users
            , let
                User{userStellarAddress = Stellar.Address address} = user
                label = userNameText user
            , Text.toLower term `isInfixOf` Text.toLower label
                || Text.toLower term `isInfixOf` Text.toLower address
            ]
