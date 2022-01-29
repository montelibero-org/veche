{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (_, User{userStellarAddress}) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userStellarAddress <> "'s User page"
        $(widgetFile "profile")
