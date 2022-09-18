{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (
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
    deleteTelegram,
) where

import Import.NoFoundation

import Database.Persist (delete, get, getBy, insert, repsert, selectList,
                         update, (=.))
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Persist (runDB)

getByStellarAddress ::
    PersistSql app => Stellar.Address -> HandlerFor app (Maybe (Entity User))
getByStellarAddress = runDB . getBy . UniqueUser

getOrInsert :: PersistSql app => User -> HandlerFor app UserId
getOrInsert record@User{userStellarAddress} =
    runDB do
        mExisted <- getBy $ UniqueUser userStellarAddress
        case mExisted of
            Just (Entity id _) -> pure id
            Nothing            -> insert record

selectAll :: PersistSql app => HandlerFor app [Entity User]
selectAll = runDB $ selectList [] []

dbSelectAll :: MonadIO m => SqlPersistT m [User]
dbSelectAll = map entityVal <$> selectList [] []

setName :: PersistSql app => UserId -> Maybe Text -> HandlerFor app ()
setName id mname = runDB $ update id [UserName =. mname]

getTelegram :: PersistSql app => UserId -> HandlerFor app (Maybe Telegram)
getTelegram uid = runDB $ get (TelegramKey uid)

dbSetTelegram :: MonadIO m => UserId -> Int64 -> Text -> SqlPersistT m ()
dbSetTelegram uid telegramChatid telegramUsername =
    repsert key Telegram{telegramChatid, telegramUsername}
  where
    key = TelegramKey uid

setTelegram :: PersistSql app => UserId -> Int64 -> Text -> HandlerFor app ()
setTelegram uid chatid username = runDB $ dbSetTelegram uid chatid username

deleteTelegram :: PersistSql app => UserId -> HandlerFor app ()
deleteTelegram uid = runDB $ delete $ TelegramKey uid
