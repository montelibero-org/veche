{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Forum (
    Forum (..),
    ForumId,
    Key (ForumKey),
    getAll,
    Model.Forum.getEntity404,
) where

import Import

import Database.Persist (selectList)
import Database.Persist.Extra qualified
import Yesod.Persist (runDB)

import Model (Forum (..), ForumId, Key (ForumKey))

getAll :: Handler [Entity Forum]
getAll = do
    requireAuthz ListForums
    runDB $ selectList [] []

getEntity404 :: ForumId -> Handler (Entity Forum)
getEntity404 = runDB . Database.Persist.Extra.getEntity404
