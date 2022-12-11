{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Authorization where

-- prelude
import Foundation.Base

-- global
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Servant (ServerError)
import Servant qualified
import Yesod.Core (HandlerSite, MonadHandler, permissionDeniedI)
import Yesod.Persist (Entity (Entity))

-- component
import Model (Issue (Issue), UserId, UserRole (UserRole))
import Model qualified
import Model.Types (EntityForum, Forum (Forum), Poll (..), Role (..), Roles)
import Model.Types qualified

-- | Use 'Entity' or 'Key' ({entity}Id)
-- when presence in the database is required.
data AuthzRequest
    = ListForums
    | ReadForum             EntityForum Roles
    | AddForumIssue         EntityForum Roles
    | ReadForumIssue        EntityForum Roles
    | AddForumIssueComment  EntityForum Roles
    | AddIssueVote      (Entity Issue)  Roles
    | EditIssue         (Entity Issue) UserId
    | CloseReopenIssue  (Entity Issue) UserId
    | AdminOp (Entity UserRole)
    | AuditOp (Entity UserRole)

isAllowed :: AuthzRequest -> Bool
isAllowed = \case
    ListForums -> True
    ReadForum               forum roles -> checkForumRoles forum roles
    AddForumIssue           forum roles -> checkForumRoles forum roles
    ReadForumIssue          forum roles -> checkForumRoles forum roles
    AddForumIssueComment    forum roles -> checkForumRoles forum roles
    AddIssueVote (Entity _ Issue{poll}) roles -> checkVote poll roles
    EditIssue        issue user -> authzEditIssue issue user
    CloseReopenIssue issue user -> authzEditIssue issue user
    AdminOp (Entity _ UserRole{role}) -> role == Admin
    AuditOp (Entity _ UserRole{role}) -> role == Audit
  where
    authzEditIssue (Entity _ Issue{author}) user = author == user

checkForumRoles :: EntityForum -> Roles -> Bool
checkForumRoles (_, Forum{requireRole}) roles = all (`elem` roles) requireRole

checkVote :: Maybe Poll -> Roles -> Bool
checkVote mPoll roles =
    case mPoll of
        Nothing              -> False
        Just ByAmountOfFcm   -> HolderOfFcm   `elem` roles
        Just ByAmountOfVeche -> HolderOfVeche `elem` roles
        Just ByMtlAmount     -> MtlHolder     `elem` roles
        Just BySignerWeight  -> MtlSigner     `elem` roles

requireAuthz :: (MonadHandler m, HandlerSite m ~ App) => AuthzRequest -> m ()
requireAuthz req = unless (isAllowed req) $ permissionDeniedI MsgUnauthorized

requireAuthzS :: MonadError ServerError m => AuthzRequest -> m ()
requireAuthzS req = unless (isAllowed req) $ throwError Servant.err403
