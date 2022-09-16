{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.StellarHolder (
    dbDelete,
    dbInsertMany,
    dbSelectAll,
) where

import Import hiding (deleteBy, keys)

import Database.Persist (deleteBy, insertMany_, selectList, (==.))
import Stellar.Horizon.Types (Asset)

dbSelectAll :: MonadIO m => Asset -> SqlPersistT m [Entity StellarHolder]
dbSelectAll asset = selectList [StellarHolderAsset ==. asset] []

dbDelete :: MonadIO m => Asset -> Text -> SqlPersistT m ()
dbDelete asset = deleteBy . UniqueHolder asset

dbInsertMany :: MonadIO m => Asset -> [Text] -> SqlPersistT m ()
dbInsertMany stellarHolderAsset keys =
    insertMany_
        [ StellarHolder{stellarHolderAsset, stellarHolderKey}
        | stellarHolderKey <- keys
        ]
