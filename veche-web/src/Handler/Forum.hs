{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Forum (
    getForumR,
    getForumsR,
) where

import Import

import Data.Map.Strict qualified as Map

import Genesis qualified
import Model.Forum qualified as Forum
import Model.Issue qualified as Issue
import Model.User (maybeAuthzRoles)
import Templates.Issue (issueTable)

getForumR :: ForumId -> Handler Html
getForumR forumId = do
    (_, roles) <- maybeAuthzRoles
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    forumE@(_, Forum{title}) <- Forum.getEntity404 forumId
    issues <- Issue.listForumIssues forumE $ Just stateOpen
    (openIssueCount, closedIssueCount) <- Issue.countOpenAndClosed forumId
    let isAddForumIssueAllowed = isAllowed $ AddForumIssue forumE roles
    defaultLayout $(widgetFile "forum")

getForumsR :: Handler Html
getForumsR = do
    (_, roles) <- maybeAuthzRoles
    let forums =
            filter (\f -> isAllowed $ ReadForum f roles) $
            Map.assocs Genesis.forums
    defaultLayout $(widgetFile "forums")
