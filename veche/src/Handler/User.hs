{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.User where

import Import

getUserR :: Handler Html
getUserR = do
    (_, User{userName, userStellarAddress}) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userStellarAddress <> "'s User page"
        $(widgetFile "user")
