{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handler.Comment where

import Import

data CommentRequest = CommentRequest{message :: Text, topic :: TopicId}
    deriving (FromJSON, Generic)

postCommentR :: Handler Value
postCommentR = do
    -- input
    CommentRequest{message, topic} <- requireCheckJsonBody
    user <- requireAuthId

    -- put comment to database
    let comment =
            Comment
                { commentAuthor = user
                , commentMessage = message
                , commentTopic = topic
                , commentParentComment = Nothing
                }
    runDB $ insert_ comment

    -- we don't need to return JSON here, but return anything just because of
    -- comme il faut, because it should be JSON API on both ends
    returnJson Null
