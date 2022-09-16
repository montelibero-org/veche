{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (
    -- * Create
    getOrInsert,
    -- * Retrieve
    getByStellarAddress,
    dbSelectAll,
    selectAll,
    -- * Update
    setName,
) where

import Import.NoFoundation

import Database.Persist (getBy, insert, selectList, update, (=.))
import Yesod.Persist (runDB)

getByStellarAddress ::
    PersistSql app => Text -> HandlerFor app (Maybe (Entity User))
getByStellarAddress = runDB . getBy . UniqueUser

getOrInsert :: PersistSql app => User -> HandlerFor app UserId
getOrInsert record@User{userStellarAddress} =
    runDB do
        mExisted <- getBy $ UniqueUser userStellarAddress
        case mExisted of
            Just (Entity id _) -> pure id
            Nothing            -> insert record

selectAll :: PersistSql app => HandlerFor app [Entity User]
selectAll = runDB $ selectList [] []

dbSelectAll :: MonadIO m => SqlPersistT m [User]
dbSelectAll = map entityVal <$> selectList [] []

setName :: PersistSql app => UserId -> Maybe Text -> HandlerFor app ()
setName id mname = runDB $ update id [UserName =. mname]
