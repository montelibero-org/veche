{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-} -- instance {Yesod,YesodBreadcrumbs...} App

module Foundation where

import Foundation.Base
import Import.NoFoundation
import Prelude qualified

-- global
import Control.Monad.Logger (LogLevel (LevelWarn), LogSource)
import Data.Time (secondsToNominalDiffTime)
import Database.Persist.Sql (SqlBackend)
import Text.Jasmine (minifym)
import Yesod.Auth.Dummy (authDummy)
import Yesod.Core (Approot (ApprootRequest),
                   AuthResult (Authorized, Unauthorized), HandlerSite,
                   SessionBackend, Yesod, YesodBreadcrumbs,
                   defaultClientSessionBackend, defaultCsrfMiddleware,
                   defaultYesodMiddleware, getApprootText, getYesod,
                   guessApproot, liftHandler)
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
import Model.User (User (User), UserId)
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
    makeSessionBackend _ =
        Just <$>
            defaultClientSessionBackend
                (8 * 24 * 60)  -- idle timeout in minutes = 1 week + 1 day
                "config/client_session_key.aes"

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
    isAuthorized route _isWrite =
        -- Actually, we check only authentication here.
        -- Authorization requires data from the database,
        -- and happens inside handlers after the data is got
        -- to minimize DB requests.
        case route of
            -- Routes not requiring authentication.
            AuthR _   -> pure Authorized
            FaviconR  -> pure Authorized
            RobotsR   -> pure Authorized
            StaticR _ -> pure Authorized
            -- All other routes require authentication.
            _         -> isAuthenticated

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
    let mw  | isAuthRMay route  = Prelude.id
            | otherwise         = defaultCsrfMiddleware
    mw h

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb = \case
        -- HomeR     -> pure ("Home",    Nothing)
        -- (AuthR _) -> pure ("Login",   Nothing)
        -- ProfileR  -> pure ("Profile", Nothing)
        _         -> pure ("",    Nothing)

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

    -- Perform authentication based on the given credentials
    authenticate
        :: (MonadHandler m, HandlerSite m ~ App)
        => Creds App -> m (AuthenticationResult App)
    authenticate Creds{credsIdent} =
        fmap Authenticated $
        liftHandler $
        User.getOrInsert
            User{name = Nothing, stellarAddress = Stellar.Address credsIdent}

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app@App{appSettings} =
        authStellar (authStellarConfig app) : extraAuthPlugins
      where

        -- Enable authDummy login if allowed.
        extraAuthPlugins = [authDummy | appAuthDummyLogin]

        AppSettings{appAuthDummyLogin} = appSettings

authStellarConfig :: App -> Yesod.Auth.Stellar.Config App
authStellarConfig App{appStellarHorizon} =
    Yesod.Auth.Stellar.Config
        { horizon = appStellarHorizon
        , setVerifyKey = \verifierUserIdent ->
            liftHandler . Verifier.setKey nonceTtl verifierUserIdent
        , checkAndRemoveVerifyKey = Verifier.checkAndRemoveVerifyKey
        }
  where
    nonceTtl = secondsToNominalDiffTime $ 60 * 15 -- 15 minutes

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    pure $ case muid of
        Nothing -> Unauthorized "You must log in to access this page"
        Just _  -> Authorized

instance YesodAuthPersist App

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

type Form = BForm Handler

defaultRoute :: Route App
defaultRoute = DashboardR

refresh :: Handler a
refresh = do
    r <- getCurrentRoute
    redirect $ fromMaybe defaultRoute r
