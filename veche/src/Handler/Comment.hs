{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handler.Comment
    ( postCommentR
    ) where

import Import

import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model.Comment qualified as Comment
import Templates.Comment (commentWidget)
import Types.Comment (CommentMaterialized (..))

data CommentRequest = CommentRequest{message :: Text, issue :: IssueId}
    deriving (FromJSON, Generic)

postCommentR :: Handler Value
postCommentR = do
    userE@(Entity _ user) <- requireAuth
    CommentRequest{message, issue} <- requireCheckJsonBody
    comment <- Comment.addText userE issue message
    returnJson $
        renderHtml $ commentWidget CommentMaterialized{author = user, comment}
