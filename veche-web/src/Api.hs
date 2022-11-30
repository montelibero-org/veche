{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-}

module Api (ApiSubsite (..), API, getSwagger) where

-- global
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Swagger (NamedSchema (NamedSchema), ToParamSchema, ToSchema,
                     declareNamedSchema)
import Data.Text (Text)
import Servant (Capture, Get, JSON, QueryParam, (:<|>), (:>))
import Servant.Server (Server)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Yesod.Core (ParseRoute, RenderRoute, Route, parseRoute, renderRoute)

-- component
import Model (Issue, IssueVersionId, UserId)
import Model.Types (Forum, ForumId, Poll, Role)

type TheAPI = "unstable" :> Unstable

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> TheAPI

type Unstable =
    "forums"
    :>  (   Get '[JSON] (Map ForumId Forum)
        :<|>
            Capture "forumId" ForumId :> "issues" :> QueryParam "open" Bool
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
    declareNamedSchema _ = pure $ NamedSchema (Just "IssueVersionId") mempty

instance ToSchema UserId where
    declareNamedSchema _ = pure $ NamedSchema (Just "UserId") mempty

getSwagger :: Server (SwaggerSchemaUI "swagger-ui" "swagger.json")
getSwagger = swaggerSchemaUIServer $ toSwagger $ Proxy @TheAPI
