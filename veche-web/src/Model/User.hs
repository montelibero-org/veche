{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (
    -- * Data
    Key (UserKey),
    Unique (UniqueUser),
    User (..),
    UserId,
    -- * Create
    getOrCreate,
    -- * Retrieve
    dbSelectAll,
    getByStellarAddress,
    getByTelegramId,
    getHolderId,
    getSignerId,
    getTelegram,
    isHolder,
    isSigner,
    selectAll,
    -- * Update
    setName,
    dbSetTelegram,
    setTelegram,
    setTelegramUsername,
    -- * Delete
    dbDeleteTelegram,
    deleteTelegram,
    -- * Authorization
    maybeAuthzRolesDB,
    maybeAuthzRolesY,
    requireAuthzRoles,
    requireAuthzRoleS,
) where

-- prelude
import Import.NoFoundation

-- global
import Control.Monad.Except (MonadError, throwError)
import Data.Set qualified as Set
import Database.Persist (delete, exists, get, getBy, insert, repsert,
                         selectFirst, selectList, update, (=.), (==.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Servant (ServerError)
import Servant qualified
import Stellar.Simple (Asset)
import Stellar.Simple qualified as Stellar
import Yesod.Core (HandlerSite, liftHandler, notAuthenticated)
import Yesod.Persist (runDB)

-- component
import Genesis (fcmAsset, mtlAsset, mtlFund, vecheAsset)
import Model (Key (TelegramKey), StellarHolder (StellarHolder), StellarHolderId,
              StellarSigner, StellarSignerId, Telegram (Telegram),
              Unique (UniqueHolder, UniqueSigner, UniqueUser), User (User),
              UserId)
import Model qualified

getByStellarAddress ::
    PersistSql app => Stellar.Address -> HandlerFor app (Maybe (Entity User))
getByStellarAddress = runDB . getBy . UniqueUser

getByTelegramId ::
    (MonadHandler m, PersistSql (HandlerSite m)) =>
    Int64 -> m (Maybe (Entity Telegram))
getByTelegramId chatid =
    liftHandler . runDB $ selectFirst [#chatid ==. chatid] []

getOrCreate ::
    (MonadHandler m, PersistSql (HandlerSite m)) => Stellar.Address -> m UserId
getOrCreate stellarAddress =
    liftHandler . runDB $ do
        mExisted <- getBy $ UniqueUser stellarAddress
        case mExisted of
            Just (Entity id _)  -> pure id
            Nothing             -> insert fresh
  where
    fresh = User{name = Nothing, stellarAddress}

selectAll :: PersistSql app => HandlerFor app [Entity User]
selectAll = runDB $ selectList [] []

dbSelectAll :: MonadIO m => SqlPersistT m [User]
dbSelectAll = map entityVal <$> selectList [] []

setName :: PersistSql app => UserId -> Maybe Text -> HandlerFor app ()
setName id mname = runDB $ update id [#name =. mname]

getTelegram :: PersistSql app => UserId -> HandlerFor app (Maybe Telegram)
getTelegram uid = runDB $ get (TelegramKey uid)

dbSetTelegram :: MonadIO m => UserId -> Int64 -> Text -> SqlPersistT m ()
dbSetTelegram uid chatid username =
    repsert key Telegram{chatid, notifyIssueAdded, username}
  where
    key = TelegramKey uid
    notifyIssueAdded = False -- default

setTelegram :: PersistSql app => UserId -> Int64 -> Text -> HandlerFor app ()
setTelegram uid chatid username = runDB $ dbSetTelegram uid chatid username

setTelegramUsername ::
    (MonadHandler m, PersistSql (HandlerSite m)) => UserId -> Text -> m ()
setTelegramUsername id username =
    liftHandler . runDB $ update (TelegramKey id) [#username =. username]

deleteTelegram :: PersistSql app => UserId -> HandlerFor app ()
deleteTelegram = runDB . dbDeleteTelegram

dbDeleteTelegram :: MonadIO m => UserId -> SqlPersistT m ()
dbDeleteTelegram = delete . TelegramKey

isSigner :: PersistSql app => User -> HandlerFor app Bool
isSigner User{stellarAddress} =
    runDB $
    exists @_ @_ @StellarSigner [#target ==. mtlFund, #key ==. stellarAddress]

isHolder :: PersistSql app => User -> Asset -> HandlerFor app Bool
isHolder User{stellarAddress} asset =
    runDB $
    exists @_ @_ @StellarHolder [#asset ==. asset, #key ==. stellarAddress]

getSignerId :: PersistSql app => User -> HandlerFor app (Maybe StellarSignerId)
getSignerId User{stellarAddress} = do
    mEntity <- runDB $ getBy $ UniqueSigner mtlFund stellarAddress
    pure $ entityKey <$> mEntity

getHolderId ::
    PersistSql app => User -> Asset -> HandlerFor app (Maybe StellarHolderId)
getHolderId User{stellarAddress} asset = do
    mEntity <- runDB $ getBy $ UniqueHolder asset stellarAddress
    pure $ entityKey <$> mEntity

-- | Get possible authz roles using Yesod.
maybeAuthzRolesY ::
    ( MonadHandler m
    , AuthId (HandlerSite m) ~ UserId
    , AuthEntity (HandlerSite m) ~ User
    , PersistSql (HandlerSite m)
    ) =>
    m (Maybe UserId, Roles)
maybeAuthzRolesY = do
    mUserE <- maybeAuth
    case mUserE of
        Nothing -> pure (Nothing, Set.empty)
        Just (Entity id User{stellarAddress}) -> do
            (_, roles) <- liftHandler $ runDB $ maybeAuthzRolesDB stellarAddress
            pure (Just id, roles)

-- | Get possible authz roles using DB.
-- We consider the address authenticated event if it's not registered.
maybeAuthzRolesDB ::
    MonadIO m => Stellar.Address -> SqlPersistT m (Maybe (Entity User), Roles)
maybeAuthzRolesDB stellarAddress = do
    mUser <- getBy $ UniqueUser stellarAddress
    signer <-
        exists
            @_ @_ @StellarSigner [#target ==. mtlFund, #key ==. stellarAddress]
    assets <-
        selectList @StellarHolder [#key ==. stellarAddress] []
        <&> map \(Entity _ StellarHolder{..}) -> asset
    pure
        ( mUser
        , Set.fromList $
            [MtlSigner | signer]
            ++ [MtlHolder     | mtlAsset   `elem` assets]
            ++ [HolderOfFcm   | fcmAsset   `elem` assets]
            ++ [HolderOfVeche | vecheAsset `elem` assets]
        )

-- | Get authz roles for Yesod.
requireAuthzRoles ::
    ( MonadHandler m
    , AuthId (HandlerSite m) ~ UserId
    , AuthEntity (HandlerSite m) ~ User
    , PersistSql (HandlerSite m)
    ) =>
    m (UserId, Roles)
requireAuthzRoles = do
    (mUserId, roles) <- maybeAuthzRolesY
    case mUserId of
        Nothing -> notAuthenticated
        Just id -> pure (id, roles)

-- | Require a specific authorization role for Servant.
requireAuthzRoleS ::
    (MonadError ServerError m, MonadIO m) =>
    ConnectionPool -> Stellar.Address -> Role -> m ()
requireAuthzRoleS pool stellarAddress role = do
    (_, roles) <- liftIO $ runSqlPool (maybeAuthzRolesDB stellarAddress) pool
    unless (role `elem` roles) $ throwError Servant.err403
