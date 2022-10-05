{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Verifier (setKey, checkAndRemoveVerifyKey) where

import Import.NoFoundation

import Data.Time (NominalDiffTime, addUTCTime)
import Database.Persist (delete, getBy, insert_)
import Database.Persist.Sql (SqlBackend)
import Stellar.Horizon.Types qualified as Stellar
import Yesod.Persist (YesodPersist, YesodPersistBackend, runDB)

import Model (Unique (UniqueVerifier), Verifier (Verifier))
import Model qualified

setKey ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    NominalDiffTime -> Stellar.Address -> Text -> HandlerFor app ()
setKey nonceTtl userIdent key = do
    now <- liftIO getCurrentTime
    let expires = addUTCTime nonceTtl now
    runDB $ insert_ Verifier{key, expires, userIdent}

checkAndRemoveVerifyKey ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    Stellar.Address -> Text -> HandlerFor app Bool
checkAndRemoveVerifyKey verifierUserIdent verifierKey =
    runDB do
        mVerifier <- getBy $ UniqueVerifier verifierUserIdent verifierKey
        case mVerifier of
            Just (Entity verifierId Verifier{expires}) -> do
                delete verifierId
                now <- liftIO getCurrentTime
                pure $ now <= expires
            Nothing -> pure False
