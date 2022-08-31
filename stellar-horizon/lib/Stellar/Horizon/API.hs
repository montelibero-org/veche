{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Stellar.Horizon.API (API, api) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Media ((//), (/:))
import Numeric.Natural (Natural)
import Servant.API (Accept, Capture, Get, JSON, MimeRender, MimeUnrender,
                    QueryParam, mimeRender, mimeUnrender, (:<|>), (:>))
import Servant.API qualified
import Servant.Docs (DocCapture (DocCapture), DocQueryParam (DocQueryParam),
                     ParamKind (Normal), ToCapture, ToParam)
import Servant.Docs qualified

import Stellar.Horizon.Types (Account, Asset, Records)

data HalJson

instance Accept HalJson where
    contentTypes _ =
        "application" // "hal+json" /: ("charset", "utf-8")
        :| ["application" // "hal+json"]

instance FromJSON a => MimeUnrender HalJson a where
    mimeUnrender _ = mimeUnrender (Proxy @JSON)

instance ToJSON a => MimeRender HalJson a where
    mimeRender _ = mimeRender (Proxy @JSON)

type API
    =       "accounts" :> Capture "account_id" Text :> Get '[HalJson] Account
    :<|>    "accounts"
            :> QueryParam "asset" Asset
            :> QueryParam "cursor" Text
            :> QueryParam "limit" Natural
            :> Get '[HalJson] (Records Account)

api :: Proxy API
api = Proxy

instance ToCapture (Capture "account_id" Text) where
    toCapture _ =
        DocCapture
            "account_id"
            "This account’s public key\
            \ encoded in a base32 string representation."

instance ToParam (QueryParam "asset" Asset) where
    toParam _ =
        DocQueryParam
            "asset"
            []
            "An issued asset represented as “Code:IssuerAccountID”.\
            \ Every account in the response will have a trustline for the given\
            \ asset."
            Normal

instance ToParam (QueryParam "cursor" Text) where
    toParam _ =
        DocQueryParam
            "cursor"
            []
            "From official docs:\
            \ A number that points to a specific location in a collection of\
            \ responses and is pulled from the `paging_token` value of a\
            \ record.\
            \ Actually arbitrary string, not a number."
            Normal

instance ToParam (QueryParam "limit" Natural) where
    toParam _ =
        DocQueryParam
            "limit"
            []
            "The maximum number of records returned.\
            \ The limit can range from 1 to 200 -\
            \ an upper limit that is hardcoded in Horizon for performance\
            \ reasons.\
            \ If this argument isn’t designated, it defaults to 10."
            Normal
