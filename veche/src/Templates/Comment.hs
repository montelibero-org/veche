{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Comment where

import Import

import Templates.User (userNameWidget)
import Types.Comment (CommentMaterialized (..))

commentWidget :: CommentMaterialized -> Html
commentWidget CommentMaterialized{id, author, comment} =
    [shamlet|
        <div ##{commentAnchor id} .panel .panel-default>
            <div .panel-heading>
                <span .comment_author>#{userNameWidget author}
                <span .comment_action>#{commentType}
                on
                <span .comment_timestamp>#{show commentCreated}
            $if commentMessage /= ""
                <div .panel-body>#{commentMessage}
    |]
  where

    Comment{commentMessage, commentType, commentCreated} = comment

commentAnchor :: CommentId -> Text
commentAnchor id = "comment" <> toPathPiece id
