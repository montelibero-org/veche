{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation.Base where

import Import.NoFoundation

import Database.Persist.Sql (ConnectionPool)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)
import Yesod.Core (mkYesodData, parseRoutesFile)
import Yesod.Core qualified
import Yesod.Core.Types (Logger)
import Yesod.Static (Static)

import Model.Issue (IssueId)

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
