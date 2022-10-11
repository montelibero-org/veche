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

import Import hiding (Value, link)

import Data.Coerce (coerce)
import Database.Esqueleto.Experimental (asc, from, innerJoin, not_, on, orderBy,
                                        select, table, unValue, where_,
                                        (:&) ((:&)), (==.), (^.))
import Database.Persist (EntityField, PersistEntity, PersistEntityBackend,
                         SymbolToField, get, getJust, update, (=.))
import Database.Persist.Sql (SqlBackend)
import Text.Shakespeare.Text (st)
import Yesod.Core (yesodRender)

import Model (Comment (Comment), CommentId, Issue (Issue), IssueId,
              Key (TelegramKey), Request (Request), Telegram, User, UserId)
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

    makeMessage :: MonadIO m => App -> Entity e -> SqlPersistT m Text
    makeMessage _ = pure . tshow

instance Event Comment where

    dbGetUsersToDeliver Comment{type_, issue, parent} =
        case type_ of
            CommentApprove  -> pure []
            CommentClose    -> pure []
            CommentEdit     -> pure []
            CommentReject   -> pure []
            CommentReopen   -> pure []
            CommentStart    -> error "not a real comment"
            CommentText     -> getParentCommentAuthor
      where
        getParentCommentAuthor =
            maybe (getIssueAuthor issue) getCommentAuthor parent

    makeMessage app (Entity id comment@Comment{author, type_, issue}) =
        case type_ of
            CommentApprove  -> dflt
            CommentClose    -> pure [st|Discussion is closed #{issueLink}|]
            CommentEdit     -> dflt
            CommentReject   -> dflt
            CommentReopen   -> pure [st|Discussion is reopened #{issueLink}|]
            CommentStart    -> error "not a real comment"
            CommentText     -> commentTextMessage
      where
        dflt = pure $ tshow comment
        issueLink = renderUrl app (IssueR issue)
        commentLink = issueLink <> "#" <> commentAnchor id

        commentTextMessage = do
            user <- getJust author
            pure [st|#{userNameText user} replied to you #{commentLink}|]

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
        select do
            user :& telegram <- from $
                table @User `innerJoin` table @Telegram
                `on` \(user :& telegram) ->
                    coerce (user ^. #id) ==. telegram ^. #id
            where_ $ user ^. #notifyIssueAdded
            pure telegram
        <&> map unwrap
      where
        unwrap :: Entity Telegram -> (UserId, Telegram)
        unwrap (Entity (TelegramKey userId) telegram) = (userId, telegram)

    makeMessage app (Entity issueId _) =
        pure [st|A new discussion is started #{renderUrl app $ IssueR issueId}|]

instance Event Request where

    dbGetUsersToDeliver Request{user} = do
        mTelegram <- get $ TelegramKey user
        pure $ toList $ (user,) <$> mTelegram

    makeMessage app (Entity _ Request{issue, comment}) = do
        Comment{author} <- getJust comment
        user            <- getJust author
        pure [st|#{userNameText user} requested you to comment on #{link}|]
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
            where_ $ not_ $ r ^. #eventDelivered
            orderBy [asc $ r ^. #created]
            pure (r ^. #created, r)
        <&> map (bimap unValue SomeEvent)

updateSetTrue ::
    (Event e, MonadIO m) => EntityField e Bool -> Key e -> SqlPersistT m ()
updateSetTrue field id = update id [field =. True]

renderUrl :: App -> Route App -> Text
renderUrl app route = yesodRender app appRoot route [] where
    App{appSettings = AppSettings{appRoot}} = app
