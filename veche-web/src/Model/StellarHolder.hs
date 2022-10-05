{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.StellarHolder (
    StellarHolder (..),
    dbDelete,
    dbInsertMany,
    dbSelectAll,
) where

import Import hiding (deleteBy, keys)

import Database.Persist (deleteBy, insertMany_, selectList, (==.))
import Stellar.Horizon.Types (Asset)
import Stellar.Horizon.Types qualified as Stellar

import Model (EntityField (StellarHolder_asset), StellarHolder (StellarHolder),
              Unique (UniqueHolder))
import Model qualified

dbSelectAll :: MonadIO m => Asset -> SqlPersistT m [Entity StellarHolder]
dbSelectAll asset = selectList [StellarHolder_asset ==. asset] []

dbDelete :: MonadIO m => Asset -> Stellar.Address -> SqlPersistT m ()
dbDelete asset = deleteBy . UniqueHolder asset

dbInsertMany :: MonadIO m => Asset -> [Stellar.Address] -> SqlPersistT m ()
dbInsertMany asset keys = insertMany_ [StellarHolder{asset, key} | key <- keys]
