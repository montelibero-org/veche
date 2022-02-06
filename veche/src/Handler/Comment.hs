{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Comment where

import Import

import Text.Blaze.Html.Renderer.Text (renderHtml)

import User (userNameWidget)

data CommentRequest = CommentRequest{message :: Text, topic :: TopicId}
    deriving (FromJSON, Generic)

commentWidget :: User -> Comment -> Html
commentWidget user Comment{commentMessage} =
    [shamlet|
        <li>
            <div .comment_author>#{userNameWidget user}
            <div .comment_message>#{commentMessage}
    |]

postCommentR :: Handler Value
postCommentR = do
    -- input
    CommentRequest{message, topic} <- requireCheckJsonBody
    (userId, user) <- requireAuthPair

    -- put comment to database
    let comment = Comment
            { commentAuthor = userId
            , commentMessage = message
            , commentTopic = topic
            , commentParentComment = Nothing
            }
    runDB $ insert_ comment

    returnJson $ renderHtml $ commentWidget user comment
