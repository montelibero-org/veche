{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.User (getBy, getOrInsert, selectValList, setName) where

import Import.NoFoundation hiding (getBy, id)

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

selectValList ::
    MonadIO m =>
    [Filter User] -> [SelectOpt User] -> ReaderT SqlBackend m [User]
selectValList filters options = map entityVal <$> selectList filters options

setName :: PersistSql app => UserId -> Maybe Text -> HandlerFor app ()
setName id mname = runDB $ update id [UserName =. mname]
