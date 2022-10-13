{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Forum (
    Forum (..),
    ForumId (ForumKey),
    getEntity404,
    getEntityByIssue404,
    getJust,
    getJustEntity,
    isPublic,
) where

import Import.NoFoundation hiding (getEntity404)

import Data.Map.Strict ((!?))
import Database.Persist (PersistException (PersistForeignConstraintUnmet))
import Yesod.Persist (runDB)
import Yesod.Persist qualified as Persist

import Genesis (forums)
import Model (Issue (Issue), IssueId)
import Model qualified

get404 :: (HasCallStack, MonadHandler m, MonadUnliftIO m) => ForumId -> m Forum
get404 id = forums !? id ?| addCallStack notFound

getEntity404 ::
    (HasCallStack, MonadHandler m, MonadUnliftIO m) => ForumId -> m EntityForum
getEntity404 id = (id,) <$> get404 id

getJust :: (HasCallStack, MonadIO m) => ForumId -> m Forum
getJust id =
    forums !? id
    ?| throwWithCallStack (PersistForeignConstraintUnmet "forum.id")

getJustEntity :: (HasCallStack, MonadIO m) => ForumId -> m EntityForum
getJustEntity id = (id,) <$> getJust id

getEntityByIssue404 :: PersistSql app => IssueId -> HandlerFor app EntityForum
getEntityByIssue404 issueId =
    runDB do
        Issue{forum = forumId} <- Persist.get404 issueId
        getJustEntity forumId

isPublic :: ForumId -> Bool
isPublic id =
    all (\Forum{requireUserGroup} -> null requireUserGroup) $ forums !? id
