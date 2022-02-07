{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Topic (getTopicR, getTopicsNewR, getTopicsR, postTopicsR) where

import Import

import Database.Persist.Sql (rawSql)
import Text.Julius (rawJS)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

import Handler.Comment (CommentMaterialized (..), commentWidget)
import User (userNameWidget)

data TopicMaterialized = TopicMaterialized
    { topic             :: Topic
    , author            :: User
    , comments          :: [CommentMaterialized]
    , currentVersion    :: TopicVersion
    }

loadTopicComments :: TopicId -> SqlPersistT Handler [CommentMaterialized]
loadTopicComments topicId = do
    comments <-
        rawSql
            "SELECT ??, ??\
            \ FROM Comment, User ON Comment.author == User.id\
            \ WHERE Comment.topic == ?"
            [toPersistValue topicId]
    pure
        [ CommentMaterialized{..}
        | (Entity _ comment, Entity _ author) <- comments
        ]

loadTopic :: TopicId -> SqlPersistT Handler TopicMaterialized
loadTopic topicId = do
    topic@Topic{topicAuthor, topicCurrentVersion} <- get404 topicId
    versionId <-
        topicCurrentVersion
        ?| lift (constraintFail "Topic.current_version must be valid")
    author <-
        get topicAuthor
        ?|> lift (constraintFail "Topic.author must exist in User table")
    comments <- loadTopicComments topicId
    currentVersion <-
        get versionId
        ?|> lift
                (constraintFail
                    "Topic.current_version must exist in TopicVersion table")
    pure TopicMaterialized{..}

(?|) :: Applicative f => Maybe a -> f a -> f a
Nothing ?| action   = action
Just x  ?| _        = pure x

(?|>) :: Monad f => f (Maybe a) -> f a -> f a
m ?|> k = m >>= (?| k)

getTopicR :: TopicId -> Handler Html
getTopicR topicId = do
    TopicMaterialized{author, comments, topic, currentVersion} <-
        runDB $ loadTopic topicId
    let Topic{topicTitle, topicOpen} = topic
    let TopicVersion{topicVersionBody} = currentVersion
    commentFormId <- newIdent
    commentListId <- newIdent
    commentTextareaId <- newIdent
    defaultLayout $(widgetFile "topic")

data NewTopic = NewTopic{title, body :: Text}

topicForm :: AForm Handler NewTopic
topicForm = do
    title <-
        areq textField (bfs ("Title" :: Text)){fsName = Just "title"} Nothing
    body <-
        unTextarea <$>
        areq
            textareaField
            (bfs ("Message" :: Text)){fsName = Just "body"}
            Nothing
    pure NewTopic{..}

getTopicsNewR :: Handler Html
getTopicsNewR = do
    (formWidget, formEnctype) <-
        generateFormPost $ renderBootstrap3 BootstrapBasicForm topicForm
    defaultLayout $(widgetFile "topics-new")

getTopicsR :: Handler Html
getTopicsR = do
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    (openTopicCount, closedTopicCount, topics) <-
        runDB $
            (,,)
            <$> count [TopicOpen ==. True]
            <*> count [TopicOpen ==. False]
            <*> selectList [TopicOpen ==. stateOpen] []
    defaultLayout $(widgetFile "topics")

postTopicsR :: Handler Html
postTopicsR = do
    ((result, formWidget), formEnctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm topicForm
    case result of
        FormSuccess topic -> do
            topicId <- addTopic topic
            redirect $ TopicR topicId
        _ -> defaultLayout $(widgetFile "topics-new")

  where

    newTopic :: Text -> UserId -> UTCTime -> Topic
    newTopic topicTitle topicAuthor topicCreated =
        Topic
            { topicTitle
            , topicAuthor
            , topicOpen = True
            , topicCreated
            , topicCurrentVersion = Nothing
            }

    addTopic :: NewTopic -> Handler TopicId
    addTopic NewTopic{title, body} = do
        now <- liftIO getCurrentTime
        user <- requireAuthId
        runDB do
            let topic = newTopic title user now
            topicId <- insert topic
            let version = TopicVersion
                    { topicVersionTopic     = topicId
                    , topicVersionBody      = body
                    , topicVersionCreated   = now
                    , topicVersionAuthor    = user
                    }
            versionId <- insert version
            update topicId [TopicCurrentVersion =. Just versionId]
            pure topicId
