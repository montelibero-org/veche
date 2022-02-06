{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module User (userNameWidget) where

import Import

import Data.Text qualified as Text

userNameWidget :: User -> Html
userNameWidget User{userName, userStellarAddress} =
    toHtml $
    case userName of
        Just name -> name
        Nothing -> "*" <> Text.takeEnd 4 userStellarAddress
