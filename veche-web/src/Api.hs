{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-}

module Api (ApiSubsite (..), API, getOpenapi) where

-- global
import Control.Lens ((?~))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiString),
                     ToParamSchema, ToSchema, type_)
import Data.OpenApi qualified
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant (Capture, Get, JSON, QueryParam, Summary, (:<|>), (:>))
import Servant.OpenApi (toOpenApi)
import Servant.Server (Server)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Yesod.Core (ParseRoute, RenderRoute, Route, parseRoute, renderRoute)

-- component
import Model (Issue, IssueVersionId, UserId)
import Model.Types (Forum, ForumId, Poll, Role)

type API = OpenAPI :<|> TheAPI

type OpenAPI = SwaggerSchemaUI "openapi-ui" "openapi.json"

type TheAPI = "unstable" :> Unstable

type Unstable =
    "forums"
    :>  (   Summary "list forums" :> Get '[JSON] (Map ForumId Forum)
        :<|>
            Summary "list issues in a forum"
            :> Capture "forumId" ForumId :> "issues" :> QueryParam "open" Bool
            :> Get '[JSON] [Issue]
        )

data ApiSubsite = ApiSubsite

instance RenderRoute ApiSubsite where

    data Route ApiSubsite = ApiR [Text] [(Text, Text)]
        deriving (Eq, Read, Show)

    renderRoute (ApiR path params) = (path, params)

instance ParseRoute ApiSubsite where
    parseRoute (path, params) = Just $ ApiR path params

instance ToParamSchema ForumId
instance ToSchema Forum
instance ToSchema ForumId
instance ToSchema Issue
instance ToSchema Poll
instance ToSchema Role

instance ToSchema IssueVersionId where
    declareNamedSchema _ =
        pure $ named "IssueVersionId" & type_ ?~ OpenApiString

instance ToSchema UserId where
    declareNamedSchema _ = pure $ named "UserId" & type_ ?~ OpenApiString

named name = NamedSchema (Just name) mempty

getOpenapi :: Server OpenAPI
getOpenapi =
    -- TODO addHeader ("Access-Control-Allow-Origin", "*") $
    swaggerSchemaUIServer $ toOpenApi (Proxy @TheAPI)
