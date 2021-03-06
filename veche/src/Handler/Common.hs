{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common handler functions.
module Handler.Common (getFaviconR, getRobotsR) where

import Import

import Data.FileEmbed (embedFile)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    pure $
        TypedContent "image/x-icon" $
        toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
    pure $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")
