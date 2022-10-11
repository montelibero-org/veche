{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (
    -- * Data
    Key (UserKey),
    User (..),
    UserId,
    -- * Create
    getOrCreate,
    -- * Retrieve
    dbSelectAll,
    getByStellarAddress,
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
    -- * Delete
    dbDeleteTelegram,
    deleteTelegram,
) where

import Import.NoFoundation

import Database.Persist (delete, exists, get, getBy, insert, repsert,
                         selectList, update, (=.), (==.))
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Persist (runDB)

import Genesis (mtlAsset, mtlFund)
import Model (Key (TelegramKey), StellarHolder, StellarHolderId, StellarSigner,
              StellarSignerId, Telegram (Telegram),
              Unique (UniqueHolder, UniqueSigner, UniqueUser), User (User),
              UserId)
import Model qualified

getByStellarAddress ::
    PersistSql app => Stellar.Address -> HandlerFor app (Maybe (Entity User))
getByStellarAddress = runDB . getBy . UniqueUser

getOrCreate :: PersistSql app => Stellar.Address -> HandlerFor app UserId
getOrCreate stellarAddress =
    runDB do
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

deleteTelegram :: PersistSql app => UserId -> HandlerFor app ()
deleteTelegram = runDB . dbDeleteTelegram

dbDeleteTelegram :: MonadIO m => UserId -> SqlPersistT m ()
dbDeleteTelegram = delete . TelegramKey

isSigner :: PersistSql app => User -> HandlerFor app Bool
isSigner User{stellarAddress} =
    runDB $
    exists @_ @_ @StellarSigner [#target ==. mtlFund, #key ==. stellarAddress]

isHolder :: PersistSql app => User -> HandlerFor app Bool
isHolder User{stellarAddress} =
    runDB $
    exists @_ @_ @StellarHolder [#asset ==. mtlAsset, #key ==. stellarAddress]

getSignerId :: PersistSql app => User -> HandlerFor app (Maybe StellarSignerId)
getSignerId User{stellarAddress} = do
    mEntity <- runDB $ getBy $ UniqueSigner mtlFund stellarAddress
    pure $ entityKey <$> mEntity

getHolderId :: PersistSql app => User -> HandlerFor app (Maybe StellarHolderId)
getHolderId User{stellarAddress} = do
    mEntity <- runDB $ getBy $ UniqueHolder mtlAsset stellarAddress
    pure $ entityKey <$> mEntity
