{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestImport
    ( module TestImport
    , module X
    ) where

import ClassyPrelude as X hiding (Handler, decodeUtf8, delete, deleteBy)

import Data.Text.Encoding qualified
import Database.Persist as X hiding (get)
import Database.Persist.Sql (Single, SqlPersistM, rawExecute, rawSql,
                             runSqlPersistMPool, unSingle)
import Test.Hspec as X
import Yesod.Auth as X
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

-- Wiping the database
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Sqlite (createSqlitePoolFromInfo, fkEnabled,
                                mkSqliteConnectionInfo, sqlDatabase)
import Lens.Micro (set)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)

import Application (makeFoundation, makeLogWare)
import Foundation as X
import Model as X
import Model.User qualified as User

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    pure (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to use a connection which has
    -- foreign key checks disabled.  Foreign key checks are enabled or disabled
    -- per connection, so this won't effect queries outside this function.
    --
    -- Aside: foreign key checks are enabled by persistent-sqlite, as of
    -- version 2.6.2, unless they are explicitly disabled in the
    -- SqliteConnectionInfo.

    let logFunc = messageLoggerSource app (appLogger app)

    let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    (`runSqlPersistMPool` pool) $ do
        tables <- getTables
        traverse_
            (`rawExecute` [])
            ["DELETE FROM " ++ escapeWith id (EntityNameDB t) | t <- tables]

getTables :: DB [Text]
getTables =
    rawSql
        @(Single Text)
        "SELECT name FROM sqlite_master WHERE type = 'table';"
        []
    <&> map unSingle

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request do
        setMethod "POST"
        addPostParam "ident" $ userStellarAddress u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> Maybe (Int64, Text) -> YesodExample App (Entity User)
createUser userStellarAddress mTelegram =
    runDB do
        user@(Entity uid _) <-
            insertEntity User{userName = Nothing, userStellarAddress}
        for_ mTelegram $ uncurry $ User.dbSetTelegram uid
        pure user

decodeUtf8Throw :: ByteString -> Text
decodeUtf8Throw = Data.Text.Encoding.decodeUtf8
