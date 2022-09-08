{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Model.Notification (dbDelete, dbSelectAll, dbInsert) where

import Import

import Database.Persist (delete, insert_)
import Database.Persist.Sql (Single, rawSql, unSingle)

dbDelete :: MonadIO m => NotificationId -> SqlPersistT m ()
dbDelete = delete

dbSelectAll ::
    MonadIO m => SqlPersistT m [(Entity Notification, Maybe Telegram)]
dbSelectAll = do
    results <-
        rawSql
            @(Entity Notification, Maybe (Single Int64), Maybe (Single Text))
            "SELECT ??, telegram.chatid, telegram.username\
            \ FROM\
                \ notification LEFT JOIN telegram\
                \ ON notification.recipient = telegram.id"
            []
    pure
        [   ( notification
            , Telegram <$> (unSingle <$> chatid) <*> (unSingle <$> username)
            )
        | (notification, chatid, username) <- results
        ]

dbInsert :: MonadIO m => UserId -> Text -> SqlPersistT m ()
dbInsert notificationRecipient notificationText =
    insert_ Notification{notificationRecipient, notificationText}
