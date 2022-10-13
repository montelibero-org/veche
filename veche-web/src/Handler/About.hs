{-# LANGUAGE TemplateHaskell #-}

module Handler.About (getAboutR) where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayout $(widgetFile "about")
