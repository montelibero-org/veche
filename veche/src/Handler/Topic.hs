{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Topic (getTopicsR) where

import Import

getTopicsR :: Handler Html
getTopicsR = do
    topics <- runDB $ selectList @Topic [] []
    defaultLayout $(widgetFile "topics")
