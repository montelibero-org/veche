{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (
    -- * Create
    getOrInsert,
    -- * Retrieve
    getBy,
    selectList,
    selectValList,
    -- * Update
    setName,
) where

import Import.NoFoundation hiding (getBy, selectList)

import Database.Persist qualified as Persist

getBy :: PersistSql app => Text -> HandlerFor app (Maybe (Entity User))
getBy = runDB . Persist.getBy . UniqueUser

getOrInsert :: PersistSql app => User -> HandlerFor app UserId
getOrInsert record@User{userStellarAddress} =
    runDB do
        mExisted <- Persist.getBy $ UniqueUser userStellarAddress
        case mExisted of
            Just (Entity id _) -> pure id
            Nothing            -> insert record

selectList :: PersistSql app => HandlerFor app [Entity User]
selectList = runDB $ Persist.selectList [] []

selectValList :: MonadIO m => SqlPersistT m [User]
selectValList = map entityVal <$> Persist.selectList [] []

setName :: PersistSql app => UserId -> Maybe Text -> HandlerFor app ()
setName id mname = runDB $ update id [UserName =. mname]
