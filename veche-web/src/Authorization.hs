{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorization where

-- global
import Data.Maybe (isJust)
import Yesod.Core (MonadHandler, permissionDenied)
import Yesod.Persist (Entity (Entity))

-- component
import Model (Forum (Forum), Issue (Issue), StellarHolderId, StellarSignerId,
              UserId)
import Model qualified
import Model.Types (AccessLevel (..))

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListForums
    | ListForumIssues
        (Entity Forum) (Maybe StellarSignerId) (Maybe StellarHolderId)
    | CreateIssue     StellarSignerId
    | ReadIssue       StellarHolderId
    | AddIssueComment StellarHolderId
    | AddVote         StellarSignerId
    | EditIssue        (Entity Issue) UserId
    | CloseReopenIssue (Entity Issue) UserId

isAllowed :: AuthzRequest -> Bool
isAllowed = \case
    ListForums -> True
    ListForumIssues (Entity _proof Forum{accessIssueRead}) mSigner mHolder ->
        case accessIssueRead of
            AccessLevelSigner       -> isJust mSigner
            AccessLevelHolder       -> isJust mHolder
            AccessLevelUninvolved   -> True
    CreateIssue     (_proof :: StellarSignerId) -> True
    ReadIssue       (_proof :: StellarHolderId) -> True
    AddIssueComment (_proof :: StellarHolderId) -> True
    AddVote         (_proof :: StellarSignerId) -> True
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
  where
    authzEditIssue (Entity _ Issue{author}) user = author == user

requireAuthz :: MonadHandler m => AuthzRequest -> m ()
requireAuthz req
    | isAllowed req = pure ()
    | otherwise     = permissionDenied ""
