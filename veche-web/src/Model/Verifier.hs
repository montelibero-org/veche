{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Verifier (setKey, checkAndRemoveVerifyKey) where

import Import.NoFoundation

import Data.Time (NominalDiffTime, addUTCTime)
import Yesod.Persist (YesodPersist, YesodPersistBackend, runDB)

setKey ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    NominalDiffTime -> Text -> Text -> HandlerFor app ()
setKey nonceTtl verifierUserIdent verifierKey = do
    now <- liftIO getCurrentTime
    let verifierExpires = addUTCTime nonceTtl now
    runDB $
        insert_ Verifier{verifierKey, verifierExpires, verifierUserIdent}

checkAndRemoveVerifyKey ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    Text -> Text -> HandlerFor app Bool
checkAndRemoveVerifyKey verifierUserIdent verifierKey =
    runDB do
        mVerifier <- getBy $ UniqueVerifier verifierUserIdent verifierKey
        case mVerifier of
            Just (Entity verifierId Verifier{verifierExpires}) -> do
                delete verifierId
                now <- liftIO getCurrentTime
                pure $ now <= verifierExpires
            Nothing -> pure False
