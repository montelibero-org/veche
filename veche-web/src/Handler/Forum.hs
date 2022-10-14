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
import Model.User (maybeAuthzGroups)
import Templates.Issue (issueTable)

getForumR :: ForumId -> Handler Html
getForumR forumId = do
    (_, groups) <- maybeAuthzGroups
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    forumE@(_, Forum{title}) <- Forum.getEntity404 forumId
    issues <- Issue.listForumIssues forumE $ Just stateOpen
    (openIssueCount, closedIssueCount) <- Issue.countOpenAndClosed forumId
    let isAddForumIssueAllowed = isAllowed $ AddForumIssue forumE groups
    defaultLayout $(widgetFile "forum")

getForumsR :: Handler Html
getForumsR = do
    (_, groups) <- maybeAuthzGroups
    let forums =
            filter (\f -> isAllowed $ ReadForum f groups) $
            Map.assocs Genesis.forums
    defaultLayout $(widgetFile "forums")
