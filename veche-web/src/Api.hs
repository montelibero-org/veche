{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Api (ApiSubsite (..), API) where

-- global
import Data.Map.Strict (Map)
import Data.Text (Text)
import Servant (Capture, Get, JSON, (:<|>), (:>))
import Yesod.Core (ParseRoute, RenderRoute, Route, parseRoute, renderRoute)

-- component
import Model (Issue)
import Model.Types (Forum, ForumId)

type API = "unstable" :> Unstable

type Unstable =
    "forums"
    :>  (   Get '[JSON] (Map ForumId Forum)
        :<|>
            Capture "forumId" ForumId
            :> Capture "open" Bool
            :> "issues"
            :> Get '[JSON] [Issue]
        )

data ApiSubsite = ApiSubsite

instance RenderRoute ApiSubsite where

    data Route ApiSubsite = ApiR [Text] [(Text, Text)]
        deriving (Eq, Read, Show)

    renderRoute (ApiR path params) = (path, params)

instance ParseRoute ApiSubsite where
    parseRoute (path, params) = Just $ ApiR path params
