{-# LANGUAGE OverloadedStrings #-}

module Handler.Stellar (getStellarFederation) where

import Import

import Genesis (escrowAddress, escrowFederatedHost)

getStellarFederation :: Handler Value
getStellarFederation = do
    queryType <- lookupGetParam "type"
    case queryType of
        Just "name" -> getName
        Just _      -> invalidArgs ["type=name expected"]
        Nothing     -> invalidArgs ["type expected"]
  where

    getName = do
        q <- lookupGetParam "q"
        case q of
            Just name -> respondName name
            Nothing -> invalidArgs ["q expected"]

    respondName name = do
        addHeader "Access-Control-Allow-Origin" "*"
        pure $
            object
                [ "stellar_address" .= (name <> "*" <> escrowFederatedHost)
                , "account_id"      .= escrowAddress
                , "memo_type"       .= ("text" :: Text)
                , "memo"            .= name
                ]
