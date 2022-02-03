{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Comment where

import Import

import Text.Blaze.Html.Renderer.Text (renderHtml)

data CommentRequest = CommentRequest{message :: Text, topic :: TopicId}
    deriving (FromJSON, Generic)

commentWidget :: User -> Comment -> Html
commentWidget User{userStellarAddress} Comment{commentMessage} =
    [shamlet|
        <li>
            <div .comment_author>#{userStellarAddress}
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
