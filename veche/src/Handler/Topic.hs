{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Topic (getTopicR, getTopicsNewR, getTopicsR, postTopicsR) where

import Import

import Text.Julius (rawJS)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

getTopicR :: TopicId -> Handler Html
getTopicR topicId = do
    (Topic{topicTitle, topicBody}, allComments) <-
        runDB $
            (,)
            <$> get404 topicId
            <*> (map entityVal <$> selectList [CommentTopic ==. topicId] [])
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
    (topicCount, topics) <-
        runDB $ (,) <$> count @_ @_ @Topic [] <*> selectList @Topic [] []
    defaultLayout $(widgetFile "topics")

postTopicsR :: Handler Html
postTopicsR = do
    ((result, formWidget), formEnctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm topicForm
    case result of
        FormSuccess NewTopic{title, body} -> do
            topicAuthor <- requireAuthId
            let topic =
                    Topic
                        { topicTitle = title
                        , topicBody = body
                        , topicAuthor
                        , topicOpen = True
                        }
            topicId <- runDB $ insert topic
            redirect $ TopicR topicId
        _ -> defaultLayout $(widgetFile "topics-new")
