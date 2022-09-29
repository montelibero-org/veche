{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.API (getApiCompleteUserR) where

import Import

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
            | Entity userId usr <- users
            , let
                User{stellarAddress = Stellar.Address address} = usr
                label = userNameText usr
            , Text.toLower term `isInfixOf` Text.toLower label
                || Text.toLower term `isInfixOf` Text.toLower address
            ]
