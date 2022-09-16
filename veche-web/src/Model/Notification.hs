{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Notification (dbDelete, dbSelectAll, dbInsert) where

import Import

import Database.Persist (delete, insert_, selectList)

dbDelete :: MonadIO m => NotificationId -> SqlPersistT m ()
dbDelete = delete

dbSelectAll :: MonadIO m => SqlPersistT m [Entity Notification]
dbSelectAll = selectList [] []

dbInsert :: MonadIO m => UserId -> Text -> SqlPersistT m ()
dbInsert notificationRecipient notificationText =
    insert_ Notification{notificationRecipient, notificationText}
