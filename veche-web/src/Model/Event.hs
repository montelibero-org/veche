{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Event (
    Event (..),
    SomeEvent (..),
    dbGetUndelivered,
) where

import Import hiding (Value)

import Database.Esqueleto.Experimental (asc, from, not_, orderBy, select, table,
                                        unValue, where_)
import Database.Persist (EntityField, PersistEntity, PersistEntityBackend,
                         SymbolToField, get, getJust, selectList, update, (=.),
                         (==.))
import Database.Persist.Sql (SqlBackend)
import Text.Hamlet (shamlet)
import Yesod.Core (yesodRender)

import Model (Comment (Comment), CommentId, Issue (Issue), IssueId,
              Key (TelegramKey), Request (Request), Telegram, UserId)
import Model qualified
import Templates.Comment (commentAnchor)
import Templates.User (userNameText)

class (PersistEntity e, PersistEntityBackend e ~ SqlBackend, Show e) => Event e
    where

    dbGetUsersToDeliver :: MonadIO m => e -> SqlPersistT m [(UserId, Telegram)]

    dbSetDelivered :: MonadIO m => Key e -> SqlPersistT m ()
    default dbSetDelivered ::
        (MonadIO m, SymbolToField "eventDelivered" e Bool) =>
        Key e -> SqlPersistT m ()
    dbSetDelivered = updateSetTrue #eventDelivered

    makeMessage :: MonadIO m => App -> Entity e -> SqlPersistT m Html
    makeMessage = defaultMessage

defaultMessage ::
    (Show e, Show (Key e), MonadIO m) => App -> Entity e -> SqlPersistT m Html
defaultMessage _ = pure . toHtml . tshow

instance Event Comment where

    dbGetUsersToDeliver Comment{author, type_, issue, parent} =
        case type_ of
            CommentAbstain      -> pure []
            CommentApprove      -> pure []
            CommentClose        -> pure []
            CommentEdit         -> pure []
            CommentReject       -> pure []
            CommentReopen       -> pure []
            CommentStart        -> error "not a real comment"
            CommentText         -> getParentCommentAuthor
            CommentTombstone    -> pure []
      where
        getParentCommentAuthor =
            maybe (getIssueAuthor issue) getCommentAuthor parent
            <&> filter (\(user, _) -> user /= author)

    makeMessage app commentE@(Entity _ Comment{type_, issue}) =
        case type_ of
            CommentAbstain  -> dflt
            CommentApprove  -> dflt
            CommentClose    -> pure [shamlet|Discussion is closed #{issueLink}|]
            CommentEdit         -> dflt
            CommentReject       -> dflt
            CommentReopen       -> reopenMessage
            CommentStart        -> error "not a real comment"
            CommentText         -> commentTextMessage app commentE
            CommentTombstone    -> dflt
      where
        dflt = defaultMessage app commentE
        issueLink = renderUrl app (IssueR issue)

        reopenMessage = pure [shamlet|Discussion is reopened #{issueLink}|]

commentTextMessage :: (MonadIO m) => App -> Entity Comment -> SqlPersistT m Html
commentTextMessage app (Entity id Comment{author, issue, message}) = do
    user <- getJust author
    pure
        [shamlet|
            #{userNameText user} replied to you:
            \
            <pre>#{excerpt}
            \
            Read and reply: #{commentLink}
            |]
  where
    issueLink = renderUrl app (IssueR issue)
    commentLink = issueLink <> "#" <> commentAnchor id
    excerpt
        | length message <= 100 = message
        | otherwise = take 100 message <> "…"

-- | issueId must present
getIssueAuthor :: MonadIO m => IssueId -> SqlPersistT m [(UserId, Telegram)]
getIssueAuthor issueId = do
    Issue{author} <- getJust issueId
    mTelegram <- get $ TelegramKey author
    pure $ toList $ (author,) <$> mTelegram

-- | commentId must present
getCommentAuthor :: MonadIO m => CommentId -> SqlPersistT m [(UserId, Telegram)]
getCommentAuthor commentId = do
    Comment{author} <- getJust commentId
    mTelegram <- get $ TelegramKey author
    pure $ toList $ (author,) <$> mTelegram

instance Event Issue where

    dbGetUsersToDeliver _ =
        selectList [#notifyIssueAdded ==. True] [] <&> map unTg
      where
        unTg :: Entity Telegram -> (UserId, Telegram)
        unTg (Entity (TelegramKey userId) telegram) = (userId, telegram)

    makeMessage app (Entity issueId _) =
        pure
            [shamlet|
                A new discussion is started #{renderUrl app $ IssueR issueId}
            |]

instance Event Request where

    dbGetUsersToDeliver Request{user} = do
        mTelegram <- get $ TelegramKey user
        pure $ toList $ (user,) <$> mTelegram

    makeMessage app (Entity _ Request{issue, comment}) = do
        Comment{author} <- getJust comment
        user            <- getJust author
        pure [shamlet|#{userNameText user} requested you to comment on #{link}|]
      where
        link = renderUrl app (IssueR issue) <> "#" <> commentAnchor comment

data SomeEvent = forall e. Event e => SomeEvent (Entity e)

-- | Get all undelivered events
dbGetUndelivered :: MonadIO m => SqlPersistT m [SomeEvent]
dbGetUndelivered =
    sequence
        [ selectUndelivered @Comment
        , selectUndelivered @Issue
        , selectUndelivered @Request
        ]
    <&> map snd . sortOn fst . concat
  where

    selectUndelivered ::
        forall r m.
        ( MonadIO m
        , Event r
        , SymbolToField "created"        r UTCTime
        , SymbolToField "eventDelivered" r Bool
        ) =>
        SqlPersistT m [(UTCTime, SomeEvent)]
    selectUndelivered =
        select do
            r <- from $ table @r
            where_ $ not_ r.eventDelivered
            orderBy [asc r.created]
            pure (r.created, r)
        <&> map (bimap unValue SomeEvent)

updateSetTrue ::
    (Event e, MonadIO m) => EntityField e Bool -> Key e -> SqlPersistT m ()
updateSetTrue field id = update id [field =. True]

renderUrl :: App -> Route App -> Text
renderUrl app route = yesodRender app appRoot route [] where
    App{appSettings = AppSettings{appRoot}} = app
