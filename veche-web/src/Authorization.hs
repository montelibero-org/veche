{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorization where

import ClassyPrelude.Yesod

import Model (Issue (..), StellarSignerId, UserId)

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListIssues      StellarSignerId
    | CreateIssue     StellarSignerId
    | ReadIssue       StellarSignerId
    | AddIssueComment StellarSignerId
    | AddVote         StellarSignerId
    | EditIssue        (Entity Issue) UserId
    | CloseReopenIssue (Entity Issue) UserId

isAllowed :: AuthzRequest -> Bool
isAllowed = \case
    ListIssues      (_proof :: StellarSignerId) -> True
    CreateIssue     (_proof :: StellarSignerId) -> True
    ReadIssue       (_proof :: StellarSignerId) -> True
    AddIssueComment (_proof :: StellarSignerId) -> True
    AddVote         (_proof :: StellarSignerId) -> True
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
  where
    authzEditIssue (Entity _ Issue{issueAuthor}) user = issueAuthor == user

requireAuthz :: MonadHandler m => AuthzRequest -> m ()
requireAuthz req
    | isAllowed req = pure ()
    | otherwise     = permissionDenied ""
