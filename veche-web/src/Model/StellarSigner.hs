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
import Yesod.Persist (runDB)

getByAddress403 :: Text -> Text -> Handler (Entity StellarSigner)
getByAddress403 target address = runDB $ getBy403 $ UniqueMember target address

selectAll :: Text -> Handler [Entity StellarSigner]
selectAll = runDB . dbSelectAll

dbSelectAll :: MonadIO m => Text -> SqlPersistT m [Entity StellarSigner]
dbSelectAll target = selectList [StellarSignerTarget ==. target] []

dbDelete :: MonadIO m => Text -> Text -> SqlPersistT m ()
dbDelete target = deleteBy . UniqueMember target

dbInsertMany :: MonadIO m => Text -> [(Text, Int)] -> SqlPersistT m ()
dbInsertMany stellarSignerTarget signers =
    insertMany_
        [ StellarSigner
            { stellarSignerTarget
            , stellarSignerKey
            , stellarSignerWeight
            }
        | (stellarSignerKey, stellarSignerWeight) <- signers
        ]

dbSetWeight :: MonadIO m => Text -> Text -> Int -> SqlPersistT m ()
dbSetWeight target key weight =
    updateWhere
        [StellarSignerTarget ==. target, StellarSignerKey ==. key]
        [StellarSignerWeight =. weight]
