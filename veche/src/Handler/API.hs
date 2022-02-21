{-# LANGUAGE OverloadedStrings #-}

module Handler.API (getApiCompleteUserR) where

import Import

getApiCompleteUserR :: Handler Value
getApiCompleteUserR =
    returnJson
        [ object ["label" .= String "hello", "value" .= String "HELLO"]
        , object ["label" .= String "world", "value" .= String "WORLD"]
        ]
