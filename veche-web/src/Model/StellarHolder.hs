{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.StellarHolder (
    dbDelete,
    dbInsertMany,
    dbSelectAll,
) where

import Import hiding (deleteBy, keys)

import Database.Persist (deleteBy)

import Genesis (mtlAsset)

dbSelectAll :: MonadIO m => SqlPersistT m [Entity StellarHolder]
dbSelectAll = selectList [StellarHolderAsset ==. mtlAsset] []

dbDelete :: MonadIO m => Text -> SqlPersistT m ()
dbDelete = deleteBy . UniqueHolder mtlAsset

dbInsertMany :: MonadIO m => [Text] -> SqlPersistT m ()
dbInsertMany keys =
    insertMany_
        [ StellarHolder{stellarHolderAsset = mtlAsset, stellarHolderKey}
        | stellarHolderKey <- keys
        ]
