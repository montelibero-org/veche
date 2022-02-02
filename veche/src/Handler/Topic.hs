{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Topic (getTopicNewR, getTopicsR) where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

data NewTopic = NewTopic{title, body :: Text}

topicForm :: AForm Handler NewTopic
topicForm = do
    title <- areq textField (bfs ("Title" :: Text)) Nothing
    body <- unTextarea <$> areq textareaField (bfs ("Text" :: Text)) Nothing
    pure NewTopic{..}

getTopicNewR :: Handler Html
getTopicNewR = do
    (formWidget, formEnctype) <-
        generateFormPost $ renderBootstrap3 BootstrapBasicForm topicForm
    defaultLayout $(widgetFile "topic-new")

getTopicsR :: Handler Html
getTopicsR = do
    (topicCount, topics) <-
        runDB $ (,) <$> count @_ @_ @Topic [] <*> selectList @Topic [] []
    defaultLayout $(widgetFile "topics")
