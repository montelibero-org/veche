{-# LANGUAGE TemplateHaskell #-}

module Handler.Pages (getPagesEscrowR) where

import Import

getPagesEscrowR :: Handler Html
getPagesEscrowR = defaultLayout $(widgetFile "pages/escrow")
