{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Api (ApiSubsite (..)) where

-- global
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant (Get, JSON, (:>))
import Servant.Server (Server, serve)
import Yesod.Core (ParseRoute, RenderRoute, Route, YesodSubDispatch, parseRoute,
                   renderRoute, yesodSubDispatch)

-- component
import Genesis (forums)
import Model.Types (Forum, ForumId)

type API = "unstable" :> Unstable

type Unstable = "forums" :> Get '[JSON] (Map ForumId Forum)

server :: Server API
server = pure forums

data ApiSubsite = ApiSubsite

instance RenderRoute ApiSubsite where

    data Route ApiSubsite = ApiR [Text] [(Text, Text)]
        deriving (Eq, Read, Show)

    renderRoute (ApiR path params) = (path, params)

instance ParseRoute ApiSubsite where
    parseRoute (path, params) = Just $ ApiR path params

instance YesodSubDispatch ApiSubsite master where
    yesodSubDispatch _ = serve (Proxy @API) server
