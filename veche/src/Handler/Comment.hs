{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Comment(
    postCommentsR,
) where

import Import

import Data.Set qualified as Set

import Model.Comment qualified as Comment
import Templates.Comment (CommentInput (..), commentAnchor, commentForm)

postCommentsR :: Handler Html
postCommentsR = do
    user <- requireAuth
    ((result, _widget), _enctype) <- runFormPost $ commentForm Nothing []
    requestUsers <- lookupRequestUsers
    case result of
        FormSuccess commentInput@CommentInput{issue} -> do
            commentId <- Comment.addText user commentInput{requestUsers}
            redirect $ IssueR issue :#: commentAnchor commentId
        _ -> invalidArgs [tshow result]

lookupRequestUsers :: Handler (Set UserId)
lookupRequestUsers = do
    requestUsers0 <- lookupPostParams "request_user"
    requestUsers1 <-
        for requestUsers0 \userIdText ->
            fromPathPiece userIdText
            ?| invalidArgs ["request_user: invalid user id: " <> userIdText]
    pure $ Set.fromList requestUsers1
