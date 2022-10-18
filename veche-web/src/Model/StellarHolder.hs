{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Model.StellarHolder (
    StellarHolder (..),
    dbDelete,
    getAll,
    dbGetAll,
    dbInsertMany,
    dbSetShare,
) where

import Import hiding (deleteBy, keys)

import Database.Persist (deleteBy, insertMany_, selectList, updateWhere, (=.),
                         (==.))
import Stellar.Horizon.Types (Asset)
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Persist (runDB)

import Model (StellarHolder (StellarHolder), Unique (UniqueHolder))
import Model qualified

dbGetAll :: MonadIO m => Asset -> SqlPersistT m [Entity StellarHolder]
dbGetAll asset = selectList [#asset ==. asset] []

getAll :: Asset -> Handler [Entity StellarHolder]
getAll asset = runDB $ selectList [#asset ==. asset] []

dbDelete :: MonadIO m => Asset -> Stellar.Address -> SqlPersistT m ()
dbDelete asset = deleteBy . UniqueHolder asset

dbInsertMany ::
    MonadIO m => Asset -> [(Stellar.Address, Decimal)] -> SqlPersistT m ()
dbInsertMany asset amounts =
    insertMany_ [StellarHolder{asset, key, amount} | (key, amount) <- amounts]

dbSetShare ::
    MonadIO m => Asset -> Stellar.Address -> Decimal -> SqlPersistT m ()
dbSetShare asset key amount =
    updateWhere
        @_ @_ @StellarHolder
        [#asset ==. asset, #key ==. key] [#amount =. amount]
