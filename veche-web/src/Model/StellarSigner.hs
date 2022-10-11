{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Model.StellarSigner (
    StellarSigner (..),
    dbDelete,
    dbInsertMany,
    dbSelectAll,
    dbSetWeight,
    getByAddress403,
    selectAll,
) where

import Import hiding (deleteBy)

import Database.Persist (deleteBy, insertMany_, selectList, updateWhere, (=.),
                         (==.))
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Persist (runDB)

import Model (StellarSigner (StellarSigner), Unique (UniqueSigner))
import Model qualified

getByAddress403 ::
    StellarMultiSigAddress -> Stellar.Address -> Handler (Entity StellarSigner)
getByAddress403 target address = runDB $ getBy403 $ UniqueSigner target address

selectAll :: StellarMultiSigAddress -> Handler [Entity StellarSigner]
selectAll = runDB . dbSelectAll

dbSelectAll ::
    MonadIO m => StellarMultiSigAddress -> SqlPersistT m [Entity StellarSigner]
dbSelectAll target = selectList [#target ==. target] []

dbDelete ::
    MonadIO m => StellarMultiSigAddress -> Stellar.Address -> SqlPersistT m ()
dbDelete target = deleteBy . UniqueSigner target

dbInsertMany ::
    MonadIO m =>
    StellarMultiSigAddress -> [(Stellar.Address, Int)] -> SqlPersistT m ()
dbInsertMany target signers =
    insertMany_ [StellarSigner{target, key, weight} | (key, weight) <- signers]

dbSetWeight ::
    MonadIO m =>
    StellarMultiSigAddress -> Stellar.Address -> Int -> SqlPersistT m ()
dbSetWeight target key weight =
    updateWhere
        @_
        @_
        @StellarSigner
        [#target ==. target, #key ==. key]
        [#weight =. weight]
