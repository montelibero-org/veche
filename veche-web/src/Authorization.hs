{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorization where

import Yesod.Core (MonadHandler, permissionDenied)
import Yesod.Persist (Entity (Entity))

import Model (Issue (Issue), StellarHolderId, StellarSignerId, UserId)
import Model qualified

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListIssues      StellarHolderId
    | CreateIssue     StellarSignerId
    | ReadIssue       StellarHolderId
    | AddIssueComment StellarHolderId
    | AddVote         StellarSignerId
    | EditIssue        (Entity Issue) UserId
    | CloseReopenIssue (Entity Issue) UserId

isAllowed :: AuthzRequest -> Bool
isAllowed = \case
    ListIssues      (_proof :: StellarHolderId) -> True
    CreateIssue     (_proof :: StellarSignerId) -> True
    ReadIssue       (_proof :: StellarHolderId) -> True
    AddIssueComment (_proof :: StellarHolderId) -> True
    AddVote         (_proof :: StellarSignerId) -> True
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
  where
    authzEditIssue (Entity _ Issue{issueAuthor}) user = issueAuthor == user

requireAuthz :: MonadHandler m => AuthzRequest -> m ()
requireAuthz req
    | isAllowed req = pure ()
    | otherwise     = permissionDenied ""
