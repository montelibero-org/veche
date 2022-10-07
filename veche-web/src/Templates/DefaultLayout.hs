{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Templates.DefaultLayout where

import Foundation.Base
import Import.NoFoundation

-- global
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Data.Version (showVersion)
import Text.Hamlet (hamletFile)
import Yesod.Core (addScript, addStylesheet, defaultCsrfCookieName,
                   defaultCsrfHeaderName, getMessage, getYesod, pageBody,
                   pageHead, pageTitle, widgetToPageContent)

-- package
import Paths_veche (version)

-- component
import Model.User (User)
import Templates.Navbar (MenuItem (MenuItem))
import Templates.Navbar qualified

defaultLayout ::
    (YesodAuthPersist App, AuthEntity App ~ User) => Widget -> Handler Html
defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    -- (title, parents) <- breadcrumbs

    -- Define the menu items of the header.
    let navbarLeftMenu =
            [ MenuItem MsgDashboard DashboardR
            , MenuItem MsgIssues    IssuesR
            ]
    let navbarRightMenu =
            concat
                [ [MenuItem MsgProfile  UserR         | isJust    muser]
                , [MenuItem MsgLogIn  $ AuthR LoginR  | isNothing muser]
                , [MenuItem MsgLogOut $ AuthR LogoutR | isJust    muser]
                ]

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <-
        widgetToPageContent do
            addScript     $ StaticR js_common_js
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_bootstrap_theme_css
            addStylesheet $ StaticR css_common_css
            addStylesheet $ StaticR css_fontawesome_all_css
            $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

isAuthRMay :: Maybe (Route App) -> Bool
isAuthRMay = \case
    Just (AuthR _)  -> True
    _               -> False
