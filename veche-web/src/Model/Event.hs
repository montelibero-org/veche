{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import Database.Persist (PersistEntity, PersistEntityBackend, get, selectList,
                         update, (=.), (==.))
import Database.Persist.Sql (SqlBackend)
import Text.Shakespeare.Text (st)
import Yesod.Core (yesodRender)
import Yesod.Persist (get404)

import Templates.Comment (commentAnchor)

class (PersistEntity e, PersistEntityBackend e ~ SqlBackend, Show e) => Event e
    where

    dbGetUsersToDeliver :: MonadIO m => e -> SqlPersistT m [(UserId, Telegram)]

    dbSetDelivered :: MonadIO m => Key e -> SqlPersistT m ()

    makeMessage :: App -> Entity e -> Text
    makeMessage _ = tshow

instance Event Comment where

    dbGetUsersToDeliver Comment{type_, issue, parent} =
        case type_ of
            CommentApprove  -> pure []
            CommentClose    -> getIssueAuthor issue
            CommentEdit     -> pure []
            CommentReject   -> pure []
            CommentReopen   -> getIssueAuthor issue
            CommentStart    -> error "not a real comment"
            CommentText     -> getParentCommentAuthor
      where
        getParentCommentAuthor =
            maybe (getIssueAuthor issue) getCommentAuthor parent

    dbSetDelivered = updateSetTrue Comment_eventDelivered

    makeMessage app (Entity id comment@Comment{type_, issue}) =
        case type_ of
            CommentApprove  -> tshow comment
            CommentClose    -> [st|Discussion is closed #{issueLink}|]
            CommentEdit     -> tshow comment
            CommentReject   -> tshow comment
            CommentReopen   -> [st|Discussion is reopened #{issueLink}|]
            CommentStart    -> error "not a real comment"
            CommentText     -> [st|Somebody replied to you #{commentLink}|]
      where
        issueLink = renderUrl app (IssueR issue)
        commentLink = issueLink <> "#" <> commentAnchor id

getIssueAuthor :: MonadIO m => IssueId -> SqlPersistT m [(UserId, Telegram)]
getIssueAuthor issueId = do
    Issue{author} <- get404 issueId
    mTelegram <- get $ TelegramKey author
    pure $ toList $ (author,) <$> mTelegram

getCommentAuthor :: MonadIO m => CommentId -> SqlPersistT m [(UserId, Telegram)]
getCommentAuthor commentId = do
    Comment{author} <- get404 commentId
    mTelegram <- get $ TelegramKey author
    pure $ toList $ (author,) <$> mTelegram

instance Event Issue where

    dbGetUsersToDeliver _ = map unwrapTgEntity <$> selectList @Telegram [] []

    dbSetDelivered = updateSetTrue Issue_eventDelivered

    makeMessage app (Entity issueId _) =
        [st|A new discussion is started #{renderUrl app $ IssueR issueId}|]

unwrapTgEntity :: Entity Telegram -> (UserId, Telegram)
unwrapTgEntity (Entity (TelegramKey userId) telegram) = (userId, telegram)

instance Event Request where

    dbGetUsersToDeliver Request{user} = do
        mTelegram <- get $ TelegramKey user
        pure $ toList $ (user,) <$> mTelegram

    dbSetDelivered = updateSetTrue Request_eventDelivered

    makeMessage app (Entity _ Request{issue = issue, comment}) =
        [st|Somebody requested you to comment in #{link}|]
      where
        link = renderUrl app (IssueR issue) <> "#" <> commentAnchor comment

data SomeEvent = forall e. Event e => SomeEvent (Entity e)

-- | Get all undelivered events
dbGetUndelivered :: MonadIO m => SqlPersistT m [SomeEvent]
dbGetUndelivered = do
    comments <- selectList [Comment_eventDelivered ==. False] []
    issues   <- selectList [Issue_eventDelivered   ==. False] []
    requests <- selectList [Request_eventDelivered ==. False] []
    pure $
        map snd $
        sortOn fst $
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
