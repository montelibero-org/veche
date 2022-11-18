{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Stellar (getStellarFederationR) where

import Import

import Genesis (escrowAddress, escrowFederatedHost)

getStellarFederationR :: Handler Value
getStellarFederationR = do
    queryType <- lookupGetParam "type"
    case queryType of
        Just "id"   -> notFound
        Just "name" -> getName
        Just _      -> invalidArgs ["type=name expected"]
        Nothing     -> invalidArgs ["type expected"]
  where

    getName = do
        q <- lookupGetParam "q"
        case q of
            Just addr -> resolveFederatedAddress addr
            Nothing -> invalidArgs ["q expected"]

    resolveFederatedAddress addr
        | let (name, rest) = break (== '*') addr
        , let host = drop 1 rest
        , host == escrowFederatedHost =
            respondName name
        | otherwise =
            notFound

    respondName name = do
        addHeader "Access-Control-Allow-Origin" "*"
        pure $
            object
                [ "stellar_address" .= (name <> "*" <> escrowFederatedHost)
                , "account_id"      .= escrowAddress
                , "memo_type"       .= ("text" :: Text)
                , "memo"            .= name
                ]
