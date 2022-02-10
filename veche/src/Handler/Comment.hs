{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
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

data CommentRequest = CommentRequest{message :: Text, issue :: IssueId}
    deriving (FromJSON, Generic)

data CommentMaterialized = CommentMaterialized
    { comment   :: Comment
    , author    :: User
    }

commentWidget :: CommentMaterialized -> Html
commentWidget CommentMaterialized{author, comment} =
    [shamlet|
        <div .panel .panel-default>
            <div .panel-heading>
                <span .comment_author>#{userNameWidget author}
                <span .comment_action>#{action}
                on
                <span .comment_timestamp>#{show commentCreated}
            $if commentMessage /= ""
                <div .panel-body>#{commentMessage}
    |]
  where

    Comment{commentMessage, commentType, commentCreated} = comment

    action :: Text
    action =
        case commentType of
            CommentClose    -> "closed issue"
            CommentEdit     -> "edited issue"
            CommentReopen   -> "reopened issue"
            CommentStart    -> "started issue"
            CommentText     -> "commented"

postCommentR :: Handler Value
postCommentR = do
    -- input
    CommentRequest{message, issue} <- requireCheckJsonBody
    (authorId, author) <- requireAuthPair
    now <- liftIO getCurrentTime

    -- put comment to database
    let comment = Comment
            { commentAuthor     = authorId
            , commentCreated    = now
            , commentMessage    = message
            , commentParent     = Nothing
            , commentIssue      = issue
            , commentType       = CommentText
            }
    runDB $ insert_ comment

    returnJson $ renderHtml $ commentWidget CommentMaterialized{author, comment}
