{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.API (getWebapiCompleteUserR) where

import Import

import Data.Text qualified as Text
import Stellar.Horizon.Client qualified as Stellar

import Model.User (User (User))
import Model.User qualified as User
import Templates.User (userNameText)

getWebapiCompleteUserR :: Handler Value
getWebapiCompleteUserR = do
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
