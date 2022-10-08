{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorization where

-- global
import Control.Monad (unless)
import Data.Maybe (isJust)
import Yesod.Core (MonadHandler, permissionDenied)
import Yesod.Persist (Entity (Entity))

-- component
import Model (Forum (Forum), Issue (Issue), StellarHolderId, StellarSignerId,
              UserId)
import Model qualified
import Model.Types (AccessLevel (..), Poll (..))

type RoleProofs = (Maybe StellarSignerId, Maybe StellarHolderId)

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListForums
    | ListForumIssues       (Entity Forum) RoleProofs
    | AddForumIssue         (Entity Forum) RoleProofs
    | ReadForumIssue        (Entity Forum) RoleProofs
    | AddForumIssueComment  (Entity Forum) RoleProofs
    | AddIssueVote (Entity Issue) (Maybe StellarSignerId)
    | EditIssue        (Entity Issue) UserId
    | CloseReopenIssue (Entity Issue) UserId

isAllowed :: AuthzRequest -> Bool
isAllowed = \case
    ListForums -> True
    ListForumIssues (Entity _proof Forum{accessIssueRead}) roles ->
        checkAccessLevel accessIssueRead roles
    AddForumIssue (Entity _proof Forum{accessIssueWrite}) roles ->
        checkAccessLevel accessIssueWrite roles
    ReadForumIssue (Entity _proof Forum{accessIssueRead}) roles ->
        checkAccessLevel accessIssueRead roles
    AddForumIssueComment (Entity _proof Forum{accessIssueComment}) roles ->
        checkAccessLevel accessIssueComment roles
    AddIssueVote (Entity _proof Issue{poll}) mSignerId ->
        checkVote poll mSignerId
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
  where
    authzEditIssue (Entity _ Issue{author}) user = author == user

checkAccessLevel :: AccessLevel -> RoleProofs -> Bool
checkAccessLevel level (mSigner, mHolder) =
    case level of
        AccessLevelSigner       -> isJust mSigner
        AccessLevelHolder       -> isJust mHolder
        AccessLevelUninvolved   -> True

checkVote :: Maybe Poll -> Maybe StellarSignerId -> Bool
checkVote mPoll mSigner =
    case mPoll of
        Nothing             -> False
        Just BySignerWeight -> isJust mSigner

requireAuthz :: MonadHandler m => AuthzRequest -> m ()
requireAuthz req = unless (isAllowed req) $ permissionDenied ""
