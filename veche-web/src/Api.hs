{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wno-orphans #-}

module Api (
    API, ApiSubsite (..), RpcRequest (..), RpcRequestBody (..), Signature (..),
    getOpenapi,
) where

-- global
import Control.Lens ((?~))
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveFromJSON)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiString),
                     ToParamSchema, ToSchema, declareNamedSchema, type_)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, OctetStream, Post, QueryParam,
                    QueryParam', ReqBody, Required, Summary, (:<|>), (:>))
import Servant.OpenApi (HasOpenApi, toOpenApi)
import Servant.Server (HasServer, Server, ServerT, hoistServerWithContext,
                       route, type (.++))
import Servant.Server qualified as Servant
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Web.HttpApiData (FromHttpApiData)
import Yesod.Core (ParseRoute, RenderRoute, Route, parseRoute, renderRoute)

-- project
import Stellar.Simple qualified as Stellar

-- component
import Model (Issue, IssueVersionId, UserId)
import Model.Types (Forum, ForumId, Poll, Role)

data RpcRequest
    = GetForumIssues{id :: ForumId, open :: Maybe Bool}
    | GetForums
    | GetSelf
    deriving (Generic)
deriveFromJSON
    Aeson.defaultOptions
        {Aeson.sumEncoding = Aeson.TaggedObject "method" "params"}
    ''RpcRequest

type API = OpenAPI :<|> TheAPI

type OpenAPI = SwaggerSchemaUI "openapi-ui" "openapi.json"

type TheAPI = "unstable" :> Unstable

newtype Signature = Signature Text -- TODO ByteString?
    deriving newtype (FromHttpApiData)
    deriving stock (Generic)

type RawReqBody = ReqBody '[OctetStream] ByteString

newtype RpcRequestBody = RpcRequestBody RawReqBody

instance
    ( HasServer sub context
    , Servant.HasContextEntry
        (context .++ Servant.DefaultErrorFormatters) Servant.ErrorFormatters
    ) =>
    HasServer (RpcRequestBody :> sub) context
    where

    type ServerT (RpcRequestBody :> sub) m = ServerT (RawReqBody :> sub) m

    route _api = route (Proxy @(RawReqBody :> sub))

    -- hoistServerWithContext :: Proxy api -> Proxy context -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n
    hoistServerWithContext _api =
        hoistServerWithContext (Proxy @(RawReqBody :> sub))

instance HasOpenApi sub => HasOpenApi (RpcRequestBody :> sub) where
    toOpenApi _ = toOpenApi $ Proxy @(ReqBody '[JSON] RpcRequest :> sub)

type Unstable
    =   Summary "RPC"
        :> QueryParam' '[Required] "user" Stellar.Address
        :> QueryParam' '[Required] "signature" Signature
        :> RpcRequestBody
        :> Post '[JSON] Value
    :<|>
        "forums"
        :>  (   Summary "list forums" :> Get '[JSON] (Map ForumId Forum)
            :<|>
                Summary "list issues in a forum"
                :> Capture "forumId" ForumId
                :> "issues"
                :> QueryParam "open" Bool
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
instance ToParamSchema Signature
instance ToParamSchema Stellar.Address

instance ToSchema Forum
instance ToSchema ForumId
instance ToSchema Issue
instance ToSchema Poll
instance ToSchema Role
instance ToSchema RpcRequest
instance ToSchema Value

instance ToSchema IssueVersionId where
    declareNamedSchema _ =
        pure $ named "IssueVersionId" & type_ ?~ OpenApiString

instance ToSchema UserId where
    declareNamedSchema _ = pure $ named "UserId" & type_ ?~ OpenApiString

named :: Text -> NamedSchema
named name = NamedSchema (Just name) mempty

getOpenapi :: Server OpenAPI
getOpenapi = swaggerSchemaUIServer $ toOpenApi (Proxy @("api" :> TheAPI))
