{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Admin (
    getAdminEventsR,
    getAdminUpdateDatabaseR,
) where

{- HLINT ignore "Fuse on/on" -}

import Import hiding (Value)

import Data.Text qualified as Text
import Data.Typeable (TypeRep, typeOf)
import Database.Esqueleto.Experimental (SqlExpr, SqlQuery, Value (Value), desc,
                                        from, innerJoin, on, orderBy, select,
                                        table, (:&) ((:&)), (==.), (^.))
import Database.Persist (selectList)
import Yesod.Persist (runDB)

import Model.Comment (Comment (Comment))
import Model.Comment qualified as Comment
import Model.Issue (Issue (Issue), IssueId)
import Model.Issue qualified
import Model.Request (Request (Request))
import Model.Request qualified
import Model.User (User, UserId)
import Model.UserRole qualified as UserRole
import Model.Vote qualified as Vote
import Templates.User (userNameText)

data Event = Event
    { created           :: UTCTime
    , type_             :: TypeRep
    , subtype           :: Text
    , forum             :: ForumId
    , issue             :: IssueId
    , author            :: Entity User
    , text              :: Text
    , eventDelivered    :: Bool
    , requestedUser     :: Maybe UserId
    }

pure [] -- hack to put Event type to the TH environment

loadComments ::
    SqlQuery
        ( SqlExpr (Entity Comment)
        , SqlExpr (Entity User)
        , SqlExpr (Value ForumId)
        )
loadComments = do
    comment :& userAuthor :& issue <- from $
        table @Comment
        `innerJoin` table @User `on` (\(comment :& user) ->
            comment ^. #author ==. user ^. #id)
        `innerJoin` table @Issue `on` \(comment :& _ :& issue) ->
            comment ^. #issue ==. issue ^. #id
    orderBy [desc $ comment ^. #created]
    pure (comment, userAuthor, issue ^. #forum)

commentToEvent :: (Entity Comment, Entity User, Value ForumId) -> Event
commentToEvent (Entity _ c@Comment{..}, userAuthor, Value forum) =
    Event
        { author        = userAuthor
        , requestedUser = Nothing
        , subtype       = Text.pack $ drop 7 $ show type_
        , text          = message
        , type_         = typeOf c
        , ..
        }

loadIssues :: SqlQuery (SqlExpr (Entity Issue), SqlExpr (Entity User))
loadIssues = do
    issue :& user <- from $
        table @Issue `innerJoin` table @User `on` \(issue :& user) ->
            issue ^. #author ==. user ^. #id
    orderBy [desc $ issue ^. #created]
    pure (issue, user)

issueToEvent :: (Entity Issue, Entity User) -> Event
issueToEvent (Entity id i@Issue{..}, userAuthor) =
    Event
        { author        = userAuthor
        , issue         = id
        , requestedUser = Nothing
        , subtype       = ""
        , text          = title
        , type_         = typeOf i
        , ..
        }

loadRequests ::
    SqlQuery
        ( SqlExpr (Entity Request)
        , SqlExpr (Entity User)
        , SqlExpr (Value ForumId)
        )
loadRequests = do
    request :& _ :& userAuthor :& issue <- from $
        table @Request
        `innerJoin` table @Comment `on` (\(request :& comment) ->
            request ^. #comment ==. comment ^. #id)
        `innerJoin` table @User `on` (\(_ :& comment :& user) ->
            comment ^. #author ==. user ^. #id)
        `innerJoin` table @Issue `on` \(request :& _ :& _ :& issue) ->
            request ^. #issue ==. issue ^. #id
    orderBy [desc $ request ^. #created]
    pure (request, userAuthor, issue ^. #forum)

requestToEvent :: (Entity Request, Entity User, Value ForumId) -> Event
requestToEvent (Entity _ r@Request{..}, author, Value forum) =
    Event
        { requestedUser = Just user
        , subtype       = ""
        , text          = ""
        , type_         = typeOf r
        , ..
        }

showTimestamp :: UTCTime -> String
showTimestamp = formatTime defaultTimeLocale "%F %T"

getAdminEventsR :: Handler Html
getAdminEventsR = do
    roleE <- UserRole.get Admin ?|> permissionDenied ""
    requireAuthz $ AdminOp roleE

    events <-
        runDB do
            comments    <- select loadComments
            issues      <- select loadIssues
            requests    <- select loadRequests
            pure $
                sortOn (Down . \Event{created} -> created) $
                    map commentToEvent comments
                    ++ map issueToEvent issues
                    ++ map requestToEvent requests
    defaultLayout $(widgetFile "admin/events")

getAdminUpdateDatabaseR :: Handler TypedContent
getAdminUpdateDatabaseR = do
    roleE <- UserRole.get Admin ?|> permissionDenied ""
    requireAuthz $ AdminOp roleE

    respondSource "text/plain" do
        issues <- lift $ runDB $ selectList [] []
        sendChunkText $
            "Updating issues "
            <> intercalate ", " (map (toPathPiece . entityKey) issues) <> "\n"
        for_ issues \issueE@(Entity issueId issueVal) -> do
            sendChunkText $ "Updating issue " <> toPathPiece issueId <> "\n"
            lift do
                Comment.updateIssueCommentNum issueId $ Just issueVal
                Vote.updateIssueApproval issueE
        sendChunkText "✅ Updated all issues\n"
