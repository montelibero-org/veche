{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Prelude qualified

-- global
import Control.Monad.Logger (LogLevel (LevelError, LevelWarn), LogSource)
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Data.Time (secondsToNominalDiffTime)
import Data.Version (showVersion)
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Client (HasHttpManager, Manager)
import Network.HTTP.Client qualified
import Servant.Client (BaseUrl)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Dummy (authDummy)
import Yesod.Core (Approot (ApprootRequest),
                   AuthResult (Authorized, Unauthorized), HandlerSite, Lang,
                   RenderMessage, SessionBackend, Yesod, YesodBreadcrumbs,
                   addScript, addStylesheet, defaultClientSessionBackend,
                   defaultCsrfCookieName, defaultCsrfHeaderName,
                   defaultCsrfMiddleware, defaultYesodMiddleware,
                   getApprootText, getMessage, getYesod, guessApproot,
                   liftHandler, mkYesodData, pageBody, pageHead, pageTitle,
                   parseRoutesFile, widgetToPageContent)
import Yesod.Core qualified
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form (FormMessage, defaultFormMessage)
import Yesod.Persist (DBRunner, YesodPersist, YesodPersistBackend,
                      YesodPersistRunner, defaultGetDBRunner)
import Yesod.Persist qualified
import Yesod.Static (Route (StaticRoute), Static, base64md5)

-- project
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Auth.Stellar (authStellar)
import Yesod.Auth.Stellar qualified

-- package
import Paths_veche (version)

-- component
import Model.User qualified as User
import Model.Verifier qualified as Verifier

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings       :: AppSettings
    , appStatic         :: Static -- ^ Settings for static file serving.
    , appConnPool       :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager    :: Manager
    , appLogger         :: Logger
    , appStellarHorizon :: BaseUrl
    }

data MenuItem = MenuItem
    { itemLabel          :: Text
    , route          :: Route App
    , accessCallback :: Bool
    }

menuItem :: Text -> Route App -> MenuItem
menuItem itemLabel route = MenuItem{itemLabel, route, accessCallback = True}

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for database access functions.
type DB a = forall m. (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing   -> getApprootText guessApproot app req
            Just root -> root

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
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        -- (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ menuItem "Dashboard" DashboardR
                , NavbarLeft $ menuItem "Issues" IssuesR
                , NavbarRight
                    (menuItem "Profile" UserR){accessCallback = isJust muser}
                , NavbarRight
                    (menuItem "Log in" $ AuthR LoginR)
                        {accessCallback = isNothing muser}
                , NavbarRight
                    (menuItem "Log out" $ AuthR LogoutR)
                        {accessCallback = isJust muser}
                ]

        let navbarLeftMenuItems  = [x | NavbarLeft  x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems =
                [x | x <- navbarLeftMenuItems, accessCallback x]
        let navbarRightFilteredMenuItems =
                [x | x <- navbarRightMenuItems, accessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <-
            widgetToPageContent $ do
                addScript     $ StaticR js_common_js
                addStylesheet $ StaticR css_bootstrap_css
                addStylesheet $ StaticR css_bootstrap_theme_css
                addStylesheet $ StaticR css_common_css
                addStylesheet $ StaticR css_fontawesome_all_css
                $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
        pure $
            appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

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

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

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

        -- Enable authDummy login if enabled.
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

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod
-- applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

isAuthRMay :: Maybe (Route App) -> Bool
isAuthRMay = \case
    Just (AuthR _)  -> True
    _               -> False

type Form = BForm Handler

defaultRoute :: Route App
defaultRoute = DashboardR

refresh :: Handler a
refresh = do
    r <- getCurrentRoute
    redirect $ fromMaybe defaultRoute r
