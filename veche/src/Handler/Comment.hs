{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Comment(
    postCommentsR,
) where

import Import

import Model.Comment qualified as Comment
import Templates.Comment (CommentInput (..), commentAnchor, commentForm)

postCommentsR :: Handler Html
postCommentsR = do
    user <- requireAuth
    ((result, _widget), _enctype) <- runFormPost $ commentForm Nothing
    case result of
        FormSuccess CommentInput{issue, message} -> do
            Entity id _ <- Comment.addText user issue message
            redirect $ IssueR issue :#: commentAnchor id
        _ -> invalidArgs [tshow result]
