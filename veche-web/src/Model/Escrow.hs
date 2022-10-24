{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}

module Model.Escrow (
    Escrow (..),
    dbGetActive,
    getActive,
) where

-- prelude
import Import

-- global
import Database.Persist (selectList, (==.))
import Yesod.Persist (runDB)

-- component
import Model (Escrow (..), IssueId)

-- | Get active (unspent) escrows for the issue
getActive :: IssueId -> Handler [Escrow]
getActive = runDB . dbGetActive

-- | Get active (unspent) escrows for the issue
dbGetActive :: MonadIO m => IssueId -> SqlPersistT m [Escrow]
dbGetActive issue =
    map entityVal <$> selectList [#issue ==. issue, #paidTx ==. Nothing] []
