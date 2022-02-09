{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Stellar.Horizon.API (API, api) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), Capture, Get, JSON, MimeRender (..),
                    MimeUnrender (..), (:>))
import Servant.Docs (DocCapture (..), ToCapture (..))

import Stellar.Horizon.Types (Account)

data HalJson

instance Accept HalJson where
    contentTypes _ =
        "application" // "hal+json" /: ("charset", "utf-8")
        :| ["application" // "hal+json"]

instance FromJSON a => MimeUnrender HalJson a where
    mimeUnrender _ = mimeUnrender (Proxy @JSON)

instance ToJSON a => MimeRender HalJson a where
    mimeRender _ = mimeRender (Proxy @JSON)

type API = "accounts" :> Capture "account_id" Text :> Get '[HalJson] Account

api :: Proxy API
api = Proxy

instance ToCapture (Capture "account_id" Text) where
    toCapture _ =
        DocCapture
            "account_id"
            "This account’s public key\
            \ encoded in a base32 string representation."
