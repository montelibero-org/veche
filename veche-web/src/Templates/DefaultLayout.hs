{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
                   defaultCsrfHeaderName, getMessages, languages, pageBody,
                   pageHead, pageTitle, widgetToPageContent)

-- package
import Paths_veche (version)

-- component
import Model.User (User)

navbarLeftMenu :: Bool -> [(AppMessage, Route App)]
navbarLeftMenu isAuthenticated =
    (   if isAuthenticated then
            (MsgDashboard, DashboardR)
        else
            (MsgAbout, AboutR)
    )
    : [(MsgForums, ForumsR)]

navbarRightMenu :: Bool -> [(AppMessage, Route App)]
navbarRightMenu isAuthenticated
    | isAuthenticated = [(MsgProfile, UserR       )]
    | otherwise       = [(MsgLogIn  , AuthR LoginR)]

-- | Translate a 'MonadHandler'-like action, e.g. a 'Widget'.
tr :: MonadHandler m => (Text, m a) -> [(Text, m a)] -> m a
tr (defaultLanguage, defaultMessage) messages = languages >>= go where
    go []                                   = defaultMessage
    go (lang : langs)
        | lang == defaultLanguage           = defaultMessage
        | Just msg <- lookup lang messages  = msg
        | otherwise                         = go langs

msgBeta :: Widget
msgBeta =
    tr
    ("en", [whamlet|
        This is a beta version of the service.
        If you find any problems, please contact
        <a href="https://t.me/cblp_su">@cblp_su
        or open an issue on
        <a href="https://github.com/montelibero-org/veche/issues/new">GitHub
    |])
    [   ("ru", [whamlet|
            Это бета-версия сервиса.
            Если вы обнаружите какие-либо проблемы, пожалуйста, свяжитесь с
            <a href="https://t.me/cblp_su">@cblp_su
            или откройте карточку на
            <a href="https://github.com/montelibero-org/veche/issues/new">GitHub
        |])
    ]

defaultLayout ::
    (YesodAuthPersist App, AuthEntity App ~ User) => Widget -> Handler Html
defaultLayout widget = do
    master <- getYesod
    sessionMessages <- getMessages

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    -- (title, parents) <- breadcrumbs

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    let navbarLeftMenu'  = navbarLeftMenu  $ isJust muser
        navbarRightMenu' = navbarRightMenu $ isJust muser

    let switchLanguage isoCode name =
            actionButton (SettingsLanguageR isoCode) ["dropdown-item"] name True

    pc <-
        widgetToPageContent do
            addScript     $ StaticR js_common_js
            addStylesheet $ StaticR css_common_css
            $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

isAuthRMay :: Maybe (Route App) -> Bool
isAuthRMay = \case
    Just (AuthR _)  -> True
    _               -> False
