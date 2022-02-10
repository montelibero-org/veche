{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorization where

import Import.NoFoundation

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListIssues      StellarSignerId
    | CreateIssue     StellarSignerId
    | ReadIssue       StellarSignerId
    | AddIssueComment StellarSignerId
    | EditIssue        (Entity Issue) UserId
    | CloseReopenIssue (Entity Issue) UserId

authorize :: AuthzRequest -> Bool
authorize = \case
    ListIssues      (_proof :: StellarSignerId) -> True
    CreateIssue     (_proof :: StellarSignerId) -> True
    ReadIssue       (_proof :: StellarSignerId) -> True
    AddIssueComment (_proof :: StellarSignerId) -> True
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
  where
    authzEditIssue (Entity _ Issue{issueAuthor}) user = issueAuthor == user

requireAuthz :: MonadHandler m => AuthzRequest -> m ()
requireAuthz req
    | authorize req = pure ()
    | otherwise     = permissionDenied ""
