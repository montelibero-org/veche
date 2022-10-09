{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Forum (
    Forum (..),
    ForumId,
    Key (ForumKey),
    getAll,
    getEntity404,
    getEntityByIssue404,
) where

import Import hiding (getEntity404)

import Database.Persist (getJustEntity, selectList)
import Database.Persist.Extra qualified
import Yesod.Persist (get404, runDB)

import Model (Forum (..), ForumId, Issue (Issue), IssueId, Key (ForumKey))
import Model qualified

getAll :: Handler [Entity Forum]
getAll = do
    requireAuthz ListForums
    runDB $ selectList [] []

getEntity404 :: ForumId -> Handler (Entity Forum)
getEntity404 = runDB . Database.Persist.Extra.getEntity404

getEntityByIssue404 :: IssueId -> Handler (Entity Forum)
getEntityByIssue404 issueId =
    runDB do
        Issue{forum = forumId} <- get404 issueId
        getJustEntity forumId
