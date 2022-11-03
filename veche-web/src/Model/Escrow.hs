{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Escrow (
    Escrow (..),
    buildIndex,
    getActive,
) where

-- prelude
import Import

-- global
import Data.Map.Strict qualified as Map

-- component
import Model (Escrow (..), IssueId)

buildIndex :: [Escrow] -> Map IssueId [Escrow]
buildIndex escrows =
    Map.fromListWith (++) [(issueId, [e]) | e@Escrow{issueId} <- escrows]

-- | Get active (unspent) escrows for the issue
getActive :: IssueId -> Handler [Escrow]
getActive issue = do
    App{appEscrowActive} <- getYesod
    fromMaybe [] . lookup issue <$> readIORef appEscrowActive
