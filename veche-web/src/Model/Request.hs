{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Model.Request (
    IssueRequestMaterialized (..),
    RequestMaterialized (..),
    selectActiveByIssueAndUser,
    selectActiveByUser,
) where

import Import

import Database.Persist (toPersistValue)
import Database.Persist.Sql (Single (..), rawSql)
import Yesod.Persist (runDB)

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

selectActiveByUser :: UserId -> Handler [RequestMaterialized]
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
