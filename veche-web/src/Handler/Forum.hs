{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Forum (
    getForumR,
    getForumsR,
) where

import Import

import Model.Forum (Forum (Forum), ForumId)
import Model.Forum qualified as Forum
import Model.Issue qualified as Issue
import Templates.Issue (issueTable)

getForumR :: ForumId -> Handler Html
getForumR forumId = do
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    forum@(Entity _ Forum{title}) <- Forum.getEntity404 forumId
    issues <- Issue.listForumIssues forum $ Just stateOpen
    (openIssueCount, closedIssueCount) <- Issue.countOpenAndClosed forumId
    defaultLayout $(widgetFile "forum")

getForumsR :: Handler Html
getForumsR = do
    forums <- Forum.getAll
    defaultLayout $(widgetFile "forums")
