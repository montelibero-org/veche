{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Comment where

import Import

import Templates.User (userNameWidget)
import Types (CommentType (..))
import Types.Comment (CommentMaterialized (..))

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
            CommentApprove      -> "approved"
            CommentClose        -> "closed issue"
            CommentEdit         -> "edited issue"
            CommentReject       -> "rejected"
            CommentReopen       -> "reopened issue"
            CommentRequestInfo  -> "requested additional information"
            CommentStart        -> "started issue"
            CommentText         -> "commented"
