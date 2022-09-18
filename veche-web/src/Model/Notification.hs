{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Model.Notification (dbDelete, dbSelectAll, dbInsert) where

import Import

import Database.Persist (delete, insert_)
import Database.Persist.Sql (Single, rawSql, unSingle)
import Prelude (id)

dbDelete :: MonadIO m => NotificationId -> SqlPersistT m ()
dbDelete = delete

dbSelectAll ::
    MonadIO m => SqlPersistT m [(Entity Notification, Maybe Telegram)]
dbSelectAll = do
    results :: [(Entity Notification, Maybe Int64, Maybe Text)] <-
        rawSql
            @(Entity Notification, Maybe (Single Int64), Maybe (Single Text))
            "SELECT ??, telegram.chatid, telegram.username\
            \ FROM\
                \ notification LEFT JOIN telegram\
                \ ON notification.recipient = telegram.id"
            []
        <&> map (trimap id (unSingle <$>) (unSingle <$>))
    pure
        [ (notification, Telegram <$> chatid <*> username)
        | (notification, chatid, username) <- results
        ]

dbInsert :: MonadIO m => UserId -> Text -> SqlPersistT m ()
dbInsert notificationRecipient notificationText =
    insert_ Notification{notificationRecipient, notificationText}

trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
trimap fa fb fc (a, b, c) = (fa a, fb b, fc c)
