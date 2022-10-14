{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Model.UserRole (get) where

import Import

import Database.Persist qualified as Persist
import Yesod.Persist (runDB)

import Model (Unique (UniqueRole), UserRole)

get :: Role -> Handler (Maybe (Entity UserRole))
get role = do
    user <- requireAuthId
    runDB $ Persist.getBy $ UniqueRole role user
