{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation.Base where

import Import.NoFoundation hiding (action)

-- global
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Client (HasHttpManager, Manager)
import Network.HTTP.Client qualified
import Servant.Client (BaseUrl)
import Yesod.Core (Lang, RenderMessage, getYesod, mkMessage, mkYesodData,
                   parseRoutesFile)
import Yesod.Core qualified
import Yesod.Core.Types (Logger)
import Yesod.Form (FormMessage, defaultFormMessage)
import Yesod.Persist (DBRunner, YesodPersist, YesodPersistRunner,
                      defaultGetDBRunner)
import Yesod.Persist qualified
import Yesod.Static (Static)

-- component
import Model (Escrow, IssueId)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appConnPool       :: ConnectionPool -- ^ Database connection pool.
    , appSettings       :: AppSettings
    , appHttpManager    :: Manager
    , appLogger         :: Logger
    , appStellarHorizon :: BaseUrl
    -- static subsites
    , appStatic     :: Static
    , appWellKnown  :: Static
    -- Stellar cache
    , appEscrowsActive :: IORef (Map IssueId [Escrow])
    }

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

mkMessage "App" "messages" "en"

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
