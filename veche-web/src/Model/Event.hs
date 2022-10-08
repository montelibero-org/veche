{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Event (
    Event (..),
    SomeEvent (..),
    dbGetUndelivered,
) where

import Import hiding (link)

import Database.Persist (PersistEntity, PersistEntityBackend, SelectOpt (Asc),
                         get, getJust, selectList, update, (=.), (==.))
import Database.Persist.Sql (SqlBackend)
import Text.Shakespeare.Text (st)
import Yesod.Core (yesodRender)

import Model (Comment (Comment), CommentId,
              EntityField (Comment_created, Comment_eventDelivered, Issue_created, Issue_eventDelivered, Request_created, Request_eventDelivered),
              Issue (Issue), IssueId, Key (TelegramKey), Request (Request),
              Telegram, UserId)
import Model qualified
import Templates.Comment (commentAnchor)
import Templates.User (userNameText)

class (PersistEntity e, PersistEntityBackend e ~ SqlBackend, Show e) => Event e
    where

    dbGetUsersToDeliver :: MonadIO m => e -> SqlPersistT m [(UserId, Telegram)]

    dbSetDelivered :: MonadIO m => Key e -> SqlPersistT m ()

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

    dbSetDelivered = updateSetTrue Comment_eventDelivered

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
        -- TODO(2022-10-09, cblp) add subscriptions
        pure []

    dbSetDelivered = updateSetTrue Issue_eventDelivered

    makeMessage app (Entity issueId _) =
        pure [st|A new discussion is started #{renderUrl app $ IssueR issueId}|]

unwrapTgEntity :: Entity Telegram -> (UserId, Telegram)
unwrapTgEntity (Entity (TelegramKey userId) telegram) = (userId, telegram)

instance Event Request where

    dbGetUsersToDeliver Request{user} = do
        mTelegram <- get $ TelegramKey user
        pure $ toList $ (user,) <$> mTelegram

    dbSetDelivered = updateSetTrue Request_eventDelivered

    makeMessage app (Entity _ Request{issue, comment}) = do
        Comment{author} <- getJust comment
        user            <- getJust author
        pure [st|#{userNameText user} requested you to comment on #{link}|]
      where
        link = renderUrl app (IssueR issue) <> "#" <> commentAnchor comment

data SomeEvent = forall e. Event e => SomeEvent (Entity e)

-- | Get all undelivered events
dbGetUndelivered :: MonadIO m => SqlPersistT m [SomeEvent]
dbGetUndelivered = do
    comments <-
        selectList [Comment_eventDelivered ==. False] [Asc Comment_created]
    issues   <-
        selectList [Issue_eventDelivered   ==. False] [Asc Issue_created  ]
    requests <-
        selectList [Request_eventDelivered ==. False] [Asc Request_created]
    pure $
        map snd . sortOn fst $
        map wrapC comments ++ map wrapI issues ++ map wrapR requests
  where
    wrapC e@(Entity _ Comment{created}) = (created, SomeEvent e)
    wrapI e@(Entity _ Issue  {created}) = (created, SomeEvent e)
    wrapR e@(Entity _ Request{created}) = (created, SomeEvent e)

updateSetTrue ::
    (Event e, MonadIO m) => EntityField e Bool -> Key e -> SqlPersistT m ()
updateSetTrue field id = update id [field =. True]

renderUrl :: App -> Route App -> Text
renderUrl app route = yesodRender app appRoot route [] where
    App{appSettings = AppSettings{appRoot}} = app
