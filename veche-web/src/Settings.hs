{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

-- prelude
import ClassyPrelude

-- global
import GHC.Stack (HasCallStack)
import Control.Exception qualified as Exception
import Data.Aeson (FromJSON, Result (Error, Success), Value, fromJSON,
                   parseJSON, withObject, (.!=), (.:), (.:?))
import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Sqlite (SqliteConf)
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Core (Route)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload,
                           widgetFileReload)
import Yesod.Static (CombineSettings, Static, combineScripts',
                     combineStylesheets')

-- project
import WithCallStack (impureThrowWithCallStack)

-- component
import Model (Corrections)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appSessionKeyFile         :: FilePath
    , appDatabaseConf           :: SqliteConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code

    , appAuthDummyLogin         :: Bool
    -- ^ Indicate if auth dummy login should be enabled.

    , appStellarHorizonUrl      :: Text
    , appTelegramBotName        :: Text
    , appTelegramBotToken       :: Text
    , appTelegramBotNotifyWhitelist :: [Text]
    -- ^ Allowed logins to deliver. For testing.
    -- When empty (default), everybody is allowed.

    , appEscrowFile :: FilePath
    -- ^ This file contains JSON-encoded 'EscrowFile'
    }

instance FromJSON AppSettings where
    parseJSON =
        withObject "AppSettings" $ \o -> do
            appStaticDir              <- o .: "static-dir"
            appSessionKeyFile         <- o .: "session-key-file"
            appDatabaseConf           <- o .: "database"
            appRoot                   <- o .: "approot"
            appHost                   <- fromString <$> o .: "host"
            appPort                   <- o .: "port"
            appIpFromHeader           <- o .: "ip-from-header"
            dev                       <- o .:? "development"      .!= False
            appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
            appShouldLogAll           <- o .:? "should-log-all"   .!= dev
            appReloadTemplates        <- o .:? "reload-templates" .!= dev
            appMutableStatic          <- o .:? "mutable-static"   .!= dev
            appSkipCombining          <- o .:? "skip-combining"   .!= dev

            appCopyright              <- o .:  "copyright"
            appAnalytics              <- o .:? "analytics"

            appAuthDummyLogin         <- o .:? "auth-dummy-login"      .!= dev

            appStellarHorizonUrl      <- o .: "stellar-horizon-url"
            appTelegramBotName        <- o .:  "telegram-bot-name"
            appTelegramBotToken       <- o .:  "telegram-bot-token"
            appTelegramBotNotifyWhitelist <-
                o .:? "telegram-bot-notify-whitelist" .!= []

            appEscrowFile <- o .: "escrow-file"

            return AppSettings{..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
    (   if appReloadTemplates compileTimeAppSettings then
            widgetFileReload
        else
            widgetFileNoReload
    )
        widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
    either Exception.throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets =
    combineStylesheets'
        (appSkipCombining compileTimeAppSettings)
        combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts =
    combineScripts' (appSkipCombining compileTimeAppSettings) combineSettings

escrowCorrections :: HasCallStack => Corrections
escrowCorrections =
    either impureThrowWithCallStack id $
    decodeEither' $(embedFile "config/escrow-corrections.yaml")
