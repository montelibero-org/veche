{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.StellarSigner (
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

getByAddress403 ::
    StellarMultiSigAddress -> Stellar.Address -> Handler (Entity StellarSigner)
getByAddress403 target address = runDB $ getBy403 $ UniqueMember target address

selectAll :: StellarMultiSigAddress -> Handler [Entity StellarSigner]
selectAll = runDB . dbSelectAll

dbSelectAll ::
    MonadIO m => StellarMultiSigAddress -> SqlPersistT m [Entity StellarSigner]
dbSelectAll target = selectList [StellarSignerTarget ==. target] []

dbDelete ::
    MonadIO m => StellarMultiSigAddress -> Stellar.Address -> SqlPersistT m ()
dbDelete target = deleteBy . UniqueMember target

dbInsertMany ::
    MonadIO m =>
    StellarMultiSigAddress -> [(Stellar.Address, Int)] -> SqlPersistT m ()
dbInsertMany stellarSignerTarget signers =
    insertMany_
        [ StellarSigner
            { stellarSignerTarget
            , stellarSignerKey
            , stellarSignerWeight
            }
        | (stellarSignerKey, stellarSignerWeight) <- signers
        ]

dbSetWeight ::
    MonadIO m =>
    StellarMultiSigAddress -> Stellar.Address -> Int -> SqlPersistT m ()
dbSetWeight target key weight =
    updateWhere
        [StellarSignerTarget ==. target, StellarSignerKey ==. key]
        [StellarSignerWeight =. weight]
