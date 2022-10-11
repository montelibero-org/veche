{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Request (
    IssueRequestMaterialized (..),
    Request (..),
    RequestMaterialized (..),
    selectActiveByIssueAndUser,
    selectActiveByUser,
) where

{- HLINT ignore "Fuse on/on" -}
{- HLINT ignore "Redundant ^." -}

import Import.NoFoundation

import Database.Esqueleto.Experimental (Value (Value), from, innerJoin, not_,
                                        on, select, table, val, where_, (&&.),
                                        (:&) ((:&)), (==.), (^.))
import Database.Persist.Sql (SqlBackend)
import Yesod.Persist (YesodPersist, YesodPersistBackend, runDB)

import Model (Comment,
              EntityField (CommentId, IssueId, RequestId, Request_comment, Request_fulfilled, Request_issue, Request_user, UserId),
              Issue, IssueId, Request (Request), RequestId, User, UserId)

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
        select do
            request :& issue :& comment :& user <- from $
                table @Request
                `innerJoin` table @Issue `on` (\(request :& issue) ->
                    request ^. Request_issue ==. issue ^. IssueId)
                `innerJoin` table @Comment `on` (\(request :& _ :& comment) ->
                    request ^. Request_comment ==. comment ^. CommentId)
                `innerJoin` table @User `on` \(request :& _ :& _ :& user) ->
                    request ^. Request_user ==. user ^. UserId
                    &&. request ^. Request_user ==. val userId
            where_ $ not_ (request ^. Request_fulfilled)
            pure (request ^. RequestId, issue, comment, user)
    pure
        [ RequestMaterialized{id, issue, comment, requestor}
        | (Value id, issue, comment, requestor) <- requests
        ]

selectActiveByIssueAndUser ::
    MonadIO m => IssueId -> UserId -> SqlPersistT m [IssueRequestMaterialized]
selectActiveByIssueAndUser issueId userId = do
    requests <-
        select do
            request :& comment :& user <- from $
                table @Request
                `innerJoin` table @Comment `on` (\(request :& comment) ->
                    request ^. Request_comment ==. comment ^. CommentId)
                `innerJoin` table @User `on` \(request :& _ :& user) ->
                    request ^. Request_user ==. user ^. UserId
                    &&. request ^. Request_user ==. val userId
            where_ $
                not_ (request ^. Request_fulfilled)
                &&. request ^. Request_issue ==. val issueId
            pure (request ^. RequestId, comment, user)
    pure
        [ IssueRequestMaterialized{id, comment, requestor}
        | (Value id, comment, requestor) <- requests
        ]
