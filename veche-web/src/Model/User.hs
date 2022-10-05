{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (
    -- * Data
    User (..),
    UserId,
    -- * Create
    getOrInsert,
    -- * Retrieve
    getByStellarAddress,
    getTelegram,
    dbSelectAll,
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

import Database.Persist (delete, get, getBy, insert, repsert, selectList,
                         update, (=.))
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Persist (runDB)

import Model (EntityField (User_name), Key (TelegramKey), Telegram (Telegram),
              Unique (UniqueUser), User (User), UserId)
import Model qualified

getByStellarAddress ::
    PersistSql app => Stellar.Address -> HandlerFor app (Maybe (Entity User))
getByStellarAddress = runDB . getBy . UniqueUser

getOrInsert :: PersistSql app => User -> HandlerFor app UserId
getOrInsert record@User{stellarAddress} =
    runDB do
        mExisted <- getBy $ UniqueUser stellarAddress
        case mExisted of
            Just (Entity id _) -> pure id
            Nothing            -> insert record

selectAll :: PersistSql app => HandlerFor app [Entity User]
selectAll = runDB $ selectList [] []

dbSelectAll :: MonadIO m => SqlPersistT m [User]
dbSelectAll = map entityVal <$> selectList [] []

setName :: PersistSql app => UserId -> Maybe Text -> HandlerFor app ()
setName id mname = runDB $ update id [User_name =. mname]

getTelegram :: PersistSql app => UserId -> HandlerFor app (Maybe Telegram)
getTelegram uid = runDB $ get (TelegramKey uid)

dbSetTelegram :: MonadIO m => UserId -> Int64 -> Text -> SqlPersistT m ()
dbSetTelegram uid chatid username = repsert key Telegram{chatid, username} where
    key = TelegramKey uid

setTelegram :: PersistSql app => UserId -> Int64 -> Text -> HandlerFor app ()
setTelegram uid chatid username = runDB $ dbSetTelegram uid chatid username

deleteTelegram :: PersistSql app => UserId -> HandlerFor app ()
deleteTelegram = runDB . dbDeleteTelegram

dbDeleteTelegram :: MonadIO m => UserId -> SqlPersistT m ()
dbDeleteTelegram = delete . TelegramKey
