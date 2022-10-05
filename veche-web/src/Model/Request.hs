{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Request (
    IssueRequestMaterialized (..),
    Request (..),
    RequestMaterialized (..),
    selectActiveByIssueAndUser,
    selectActiveByUser,
) where

import Import.NoFoundation

import Database.Persist (toPersistValue)
import Database.Persist.Sql (Single (Single), SqlBackend, rawSql)
import Yesod.Persist (YesodPersist, YesodPersistBackend, runDB)

import Model (Comment, Issue, IssueId, Request (Request), RequestId, User,
              UserId)

-- | A request for specific user
data RequestMaterialized = RequestMaterialized
    { id        :: RequestId
    , issue     :: Entity Issue
    , comment   :: Entity Comment
    , requestor :: Entity User
    }
    deriving (Eq, Show)

-- | A request for specific user in the context of some issue
data IssueRequestMaterialized = IssueRequestMaterialized
    { id        :: RequestId
    , comment   :: Entity Comment
    , requestor :: Entity User
    }
    deriving (Eq, Show)

selectActiveByUser ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    UserId -> HandlerFor app [RequestMaterialized]
selectActiveByUser userId = do
    requests <-
        runDB $
        rawSql
            @(Single RequestId, Entity Issue, Entity Comment, Entity User)
            "SELECT request.id, ??, ??, ??\
            \ FROM request\
            \ JOIN issue ON request.issue = issue.id\
            \ JOIN comment ON request.comment = comment.id\
            \ JOIN user ON request.user = user.id\
            \ WHERE request.fulfilled = FALSE AND request.user = ?"
            [toPersistValue userId]
    pure
        [ RequestMaterialized{id, issue, comment, requestor}
        | (Single id, issue, comment, requestor) <- requests
        ]

selectActiveByIssueAndUser ::
    MonadIO m => IssueId -> UserId -> SqlPersistT m [IssueRequestMaterialized]
selectActiveByIssueAndUser issueId userId = do
    requests <-
        rawSql
            @(Single RequestId, Entity Comment, Entity User)
            "SELECT request.id, ??, ??\
            \ FROM request\
            \ JOIN comment ON request.comment = comment.id\
            \ JOIN user ON request.user = user.id\
            \ WHERE   request.fulfilled = FALSE\
                \ AND request.issue = ?\
                \ AND request.user = ?"
            [toPersistValue issueId, toPersistValue userId]
    pure
        [ IssueRequestMaterialized{id, comment, requestor}
        | (Single id, comment, requestor) <- requests
        ]
