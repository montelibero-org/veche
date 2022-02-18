{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (getBy, getOrInsert, selectValList) where

import ClassyPrelude.Yesod hiding (getBy, id)

import Database.Persist qualified as Persist

import Model (Unique (UniqueUser), User (..), UserId)

type YesodPersistSql app =
    (YesodPersist app, BaseBackend (YesodPersistBackend app) ~ SqlBackend)

-- type PersistQueryRead' app =
--     (YesodPersistSql app, PersistQueryRead (YesodPersistBackend app))

type PersistStoreWrite' app =
    (YesodPersistSql app, PersistStoreWrite (YesodPersistBackend app))

type PersistUniqueRead' app =
    (YesodPersistSql app, PersistUniqueRead (YesodPersistBackend app))

getBy :: PersistUniqueRead' app => Text -> HandlerFor app (Maybe (Entity User))
getBy = runDB . Persist.getBy . UniqueUser

getOrInsert ::
    (PersistUniqueRead' app, PersistStoreWrite' app) =>
    User -> HandlerFor app UserId
getOrInsert record@User{userStellarAddress} =
    runDB do
        mExisted <- Persist.getBy $ UniqueUser userStellarAddress
        case mExisted of
            Just (Entity id _) -> pure id
            Nothing            -> insert record

selectValList ::
    MonadIO m =>
    [Filter User] -> [SelectOpt User] -> ReaderT SqlBackend m [User]
selectValList filters options = map entityVal <$> selectList filters options
