{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Model.Request (
    IssueRequestMaterialized (..),
    RequestMaterialized (..),
    selectActiveByIssueAndUser,
    selectByUser,
) where

import Import

import Database.Persist.Sql (Single (..), rawSql)

-- | A request for specific user
data RequestMaterialized = RequestMaterialized
    { id        :: RequestId
    , issue     :: Entity Issue
    , comment   :: Entity Comment
    , requestor :: Entity User
    }
    deriving (Eq, Show)

-- | A request for specific user and specific issue
data IssueRequestMaterialized = IssueRequestMaterialized
    { id        :: RequestId
    , comment   :: Entity Comment
    , requestor :: Entity User
    }
    deriving (Eq, Show)

selectByUser :: UserId -> Handler [RequestMaterialized]
selectByUser userId = do
    requests <-
        runDB $
            rawSql
                @(Single RequestId, Entity Issue, Entity Comment, Entity User)
                "SELECT request.id, ??, ??, ??\
                \ FROM request\
                \ JOIN issue ON request.issue = issue.id\
                \ JOIN comment ON request.comment = comment.id\
                \ JOIN user ON request.user = user.id\
                \ WHERE request.user = ?"
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
            \ WHERE   request.fulfilled = 0\
                \ AND request.issue = ?\
                \ AND request.user = ?"
            [toPersistValue issueId, toPersistValue userId]
    pure
        [ IssueRequestMaterialized{id, comment, requestor}
        | (Single id, comment, requestor) <- requests
        ]
