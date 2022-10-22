{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Request (
    IssueRequestMaterialized (..),
    Request (..),
    RequestMaterialized (..),
    selectActiveByIssueAndRequestedUser,
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

import Model (Comment, Issue, IssueId, Request (..), RequestId, User, UserId)

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
                    request ^. #issue ==. issue ^. #id)
                `innerJoin` table @Comment `on` (\(request :& _ :& comment) ->
                    request ^. #comment ==. comment ^. #id)
                `innerJoin` table @User `on` \(request :& _ :& _ :& user) ->
                    request ^. #user ==. user ^. #id
                    &&. request ^. #user ==. val userId
            where_ $ not_ (request ^. #fulfilled)
            pure (request ^. #id, issue, comment, user)
    pure
        [ RequestMaterialized{id, issue, comment, requestor}
        | (Value id, issue, comment, requestor) <- requests
        ]

selectActiveByIssueAndRequestedUser ::
    MonadIO m => IssueId -> UserId -> SqlPersistT m [IssueRequestMaterialized]
selectActiveByIssueAndRequestedUser issueId requestedUserId = do
    requests <-
        select do
            request :& comment :& user <- from $
                table @Request
                `innerJoin` table @Comment `on` (\(request :& comment) ->
                    request ^. #comment ==. comment ^. #id)
                `innerJoin` table @User `on` \(request :& comment :& user) ->
                    comment ^. #author ==. user ^. #id
                    &&. request ^. #user ==. val requestedUserId
            where_ $
                not_ (request ^. #fulfilled)
                &&. request ^. #issue ==. val issueId
            pure (request ^. #id, comment, user)
    pure
        [ IssueRequestMaterialized{id, comment, requestor}
        | (Value id, comment, requestor) <- requests
        ]
