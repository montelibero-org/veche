{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorization where

-- global
import Control.Monad (unless)
import Yesod.Core (MonadHandler, permissionDenied)
import Yesod.Persist (Entity (Entity))

-- component
import Model (Issue (Issue), UserId)
import Model qualified
import Model.Types (EntityForum, Forum (Forum), Poll (..), UserGroup (Signers),
                    UserGroups)
import Model.Types qualified

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListForums
    | ReadForum             EntityForum UserGroups
    | AddForumIssue         EntityForum UserGroups
    | ReadForumIssue        EntityForum UserGroups
    | AddForumIssueComment  EntityForum UserGroups
    | AddIssueVote      (Entity Issue)  UserGroups
    | EditIssue         (Entity Issue) UserId
    | CloseReopenIssue  (Entity Issue) UserId

isAllowed :: AuthzRequest -> Bool
isAllowed = \case
    ListForums -> True
    ReadForum               forum groups -> checkForumGroups forum groups
    AddForumIssue           forum groups -> checkForumGroups forum groups
    ReadForumIssue          forum groups -> checkForumGroups forum groups
    AddForumIssueComment    forum groups -> checkForumGroups forum groups
    AddIssueVote (Entity _ Issue{poll}) groups -> checkVote poll groups
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
  where
    authzEditIssue (Entity _ Issue{author}) user = author == user

checkForumGroups :: EntityForum -> UserGroups -> Bool
checkForumGroups (_, Forum{requireUserGroup}) groups =
    all (`elem` groups) requireUserGroup

checkVote :: Maybe Poll -> UserGroups -> Bool
checkVote mPoll groups =
    case mPoll of
        Nothing             -> False
        Just BySignerWeight -> Signers `elem` groups

requireAuthz :: MonadHandler m => AuthzRequest -> m ()
requireAuthz req = unless (isAllowed req) $ permissionDenied ""
