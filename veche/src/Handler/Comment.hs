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

data CommentMaterialized = CommentMaterialized
    { comment   :: Comment
    , author    :: User
    }

commentWidget :: CommentMaterialized -> Html
commentWidget CommentMaterialized{author, comment=Comment{commentMessage}} =
    [shamlet|
        <li>
            <div .comment_author>#{userNameWidget author}
            <div .comment_message>#{commentMessage}
    |]

postCommentR :: Handler Value
postCommentR = do
    -- input
    CommentRequest{message, topic} <- requireCheckJsonBody
    (authorId, author) <- requireAuthPair

    -- put comment to database
    let comment = Comment
            { commentAuthor = authorId
            , commentMessage = message
            , commentTopic = topic
            , commentParentComment = Nothing
            }
    runDB $ insert_ comment

    returnJson $ renderHtml $ commentWidget CommentMaterialized{author, comment}
