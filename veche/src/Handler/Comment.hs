{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Comment
    ( CommentMaterialized (..)
    , commentWidget
    , postCommentR
    ) where

import Import

import Text.Blaze.Html.Renderer.Text (renderHtml)

import Handler.User (userNameWidget)
import Types (CommentType (..))

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
            , commentType = CommentText
            }
    runDB $ insert_ comment

    returnJson $ renderHtml $ commentWidget CommentMaterialized{author, comment}
