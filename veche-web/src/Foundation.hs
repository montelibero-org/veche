{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-} -- instance {Yesod,YesodAuth...} App

module Foundation where

-- prelude
import Foundation.Base
import Import.NoFoundation

-- global
import Control.Monad.Logger (LogLevel (LevelWarn), LogSource)
import Data.Text qualified as Text
import Database.Persist.Sql (SqlBackend)
import Text.Jasmine (minifym)
import Text.Read (readEither)
import Yesod.Auth.Dummy (authDummy)
import Yesod.Auth.Message (AuthMessage (LoginTitle))
import Yesod.Core (Approot (ApprootRequest), AuthResult (Authorized),
                   HandlerSite, SessionBackend, Yesod, addMessage,
                   defaultClientSessionBackend, defaultCsrfMiddleware,
                   defaultYesodMiddleware, getApprootText, getRouteToParent,
                   getYesod, guessApproot, unauthorizedI)
import Yesod.Core qualified
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static (Route (StaticRoute), base64md5)

-- project
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Auth.Stellar (authStellar)
import Yesod.Auth.Stellar qualified

-- component
import Authentication.Telegram (authTelegram)
import Model.Forum qualified as Forum
import Model.Telegram (Key (TelegramKey), Telegram (Telegram))
import Model.Telegram qualified
import Model.User (UserId)
import Model.User qualified as User
import Model.Verifier qualified as Verifier
import Templates.DefaultLayout (isAuthRMay)
import Templates.DefaultLayout qualified

-- | A convenient synonym for database access functions.
type DB a = forall m. (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ getApprootText guessApproot

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend App{appSettings = AppSettings{appSessionKeyFile}} =
        Just <$>
            defaultClientSessionBackend
                (8 * 24 * 60)  -- idle timeout in minutes = 1 week + 1 day
                appSessionKeyFile
                -- "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    -- yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware . csrfMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout = Templates.DefaultLayout.defaultLayout

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    isAuthorized route _isWrite = isAuthorized route

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . (`StaticRoute` []))
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        pure $ appShouldLogAll (appSettings app) || level >= LevelWarn

    makeLogger :: App -> IO Logger
    makeLogger = pure . appLogger

csrfMiddleware :: Handler a -> Handler a
csrfMiddleware h = do
    route <- getCurrentRoute
    let mw  | isAuthRMay route  = identity
            | otherwise         = defaultCsrfMiddleware
    mw h

-- -- Define breadcrumbs.
-- instance YesodBreadcrumbs App where
--     -- Takes the route that the user is currently on, and returns a tuple
--     -- of the 'Text' that you want the label to display, and a previous
--     -- breadcrumb route.
--     breadcrumb
--         :: Route App  -- ^ The route the user is visiting currently.
--         -> Handler (Text, Maybe (Route App))
--     breadcrumb = \case
--         -- HomeR     -> pure ("Home",    Nothing)
--         -- (AuthR _) -> pure ("Login",   Nothing)
--         -- ProfileR  -> pure ("Profile", Nothing)
--         _         -> pure ("",    Nothing)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = DashboardR

    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = AuthR LoginR

    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    -- Record authentication based on the given verified credentials
    authenticate
        :: (MonadHandler m, HandlerSite m ~ App)
        => Creds App -> m (AuthenticationResult App)
    authenticate Creds{credsPlugin, credsIdent, credsExtra} =
        case credsPlugin of
            "dummy"     -> authenticateStellar credsIdent
            "stellar"   -> authenticateStellar credsIdent
            "telegram"  -> authenticateTelegram credsIdent credsExtra
            _           -> pure $ ServerError "Unknown auth plugin"

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app@App{appSettings} =
        authStellar (authStellarConfig app)
        : authTelegram
        : [authDummy | appAuthDummyLogin]
      where
        AppSettings{appAuthDummyLogin} = appSettings

    loginHandler = do
        routeToParent <- getRouteToParent
        authLayout do
            setTitleI LoginTitle
            master <- getYesod
            let plugins =
                    [   ( lookup apName labels & fromMaybe apName
                        , apLogin routeToParent
                        )
                    | AuthPlugin{apName, apLogin} <- authPlugins master
                    ]
            [whamlet|
                <div .form-horizontal>
                    $forall (label, login) <- plugins
                        <div .form-group>
                            <label .col-sm-4 .control-label>#{label}
                            <div .col-sm-8>
                                ^{login}
            |]
      where
        labels =
            [ ("stellar", "Via Stellar")
            , ("telegram", "Via Telegram (only for existing accounts)")
            ]

authenticateStellar ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Text -> m (AuthenticationResult App)
authenticateStellar credsIdent =
    Authenticated <$> User.getOrCreate (Stellar.Address credsIdent)

authenticateTelegram ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Text -> [(Text, Text)] -> m (AuthenticationResult App)
authenticateTelegram credsIdent credsExtra = do
    mUserTelegram <- User.getByTelegramId telegramId
    case mUserTelegram of
        Nothing -> do
            addMessage
                "danger"
                "This telegram account is not bound to any registered user.\
                    \ Please register via Stellar first."
            redirect $ AuthR LoginR
        Just (Entity (TelegramKey userId) Telegram{username}) -> do
            when (username /= authenticatedUsername) $
                User.setTelegramUsername userId authenticatedUsername
            pure $ Authenticated userId
  where
    telegramId = either error identity $ readEither $ Text.unpack credsIdent
    authenticatedUsername = lookup "username" credsExtra & fromMaybe (error "")

authStellarConfig :: App -> Yesod.Auth.Stellar.Config App
authStellarConfig App{appStellarHorizon} =
    Yesod.Auth.Stellar.Config
        { horizon = appStellarHorizon
        , getVerifyKey = Verifier.getKey
        , checkAndRemoveVerifyKey = Verifier.checkAndRemoveKey
        }

-- Actually, we check only authentication here.
-- Authorization requires data from the database,
-- and happens inside handlers after the data is got to minimize DB requests.
isAuthorized :: Route App -> HandlerFor App AuthResult
isAuthorized = \case
    -- Routes not requiring authentication.
    AboutR{}                -> authorized
    AuthR{}                 -> authorized
    FaviconR                -> authorized
    ForumsR{}               -> authorized
    IssueR{}                -> authorized
    RobotsR                 -> authorized
    RootR                   -> authorized
    StaticR{}               -> authorized
    StellarFederationR{}    -> authorized
    WellKnownR{}            -> authorized
    -- Some forums are public
    ForumR id
        | Forum.isPublic id -> authorized
        | otherwise         -> authorizedIfAuthenticated
    -- All other routes require authentication.
    _ -> authorizedIfAuthenticated
  where
    authorized = pure Authorized

-- | Access function to determine if a user is logged in.
authorizedIfAuthenticated :: Handler AuthResult
authorizedIfAuthenticated = do
    muid <- maybeAuthId
    case muid of
        Nothing -> unauthorizedI MsgUnauthorized
        Just _  -> pure Authorized

instance YesodAuthPersist App

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

type Form = BForm Handler
