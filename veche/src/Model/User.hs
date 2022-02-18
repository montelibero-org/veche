{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (getBy, getOrInsert) where

import ClassyPrelude.Yesod hiding (getBy, id)

import Database.Persist qualified as Persist

import Model (Unique (UniqueUser), User (..), UserId)

type YesodSql app = (YesodPersist app, YesodPersistBackend app ~ SqlBackend)

getBy :: YesodSql app => Text -> HandlerFor app (Maybe (Entity User))
getBy = runDB . Persist.getBy . UniqueUser

getOrInsert :: YesodSql app => User -> HandlerFor app UserId
getOrInsert record@User{userStellarAddress} =
    runDB do
        mExisted <- Persist.getBy $ UniqueUser userStellarAddress
        case mExisted of
            Just (Entity id _) -> pure id
            Nothing            -> insert record
