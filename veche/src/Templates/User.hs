{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.User where

import Import

import Data.Text qualified as Text

userNameWidget :: User -> Html
userNameWidget User{userName, userStellarAddress} =
    toHtml $
    case userName of
        Just name -> name <> " (" <> abbreviatedAddress <> ")"
        Nothing -> abbreviatedAddress
  where
    abbreviatedAddress = "*" <> Text.takeEnd 4 userStellarAddress
