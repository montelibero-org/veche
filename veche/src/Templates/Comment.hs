{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Comment (
    CommentInput (..),
    commentAnchor,
    commentForm,
    commentWidget,
) where

import Import

-- global
import Data.Set qualified as Set
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapHorizontalForm),
                              BootstrapGridOptions (ColSm), bfs,
                              renderBootstrap3)

-- component
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

data CommentInput = CommentInput
    { issue        :: IssueId
    , message      :: Text
    , requestUsers :: Set UserId
    }
    deriving Show

commentForm ::
    Maybe IssueId -> (Html -> MForm Handler (FormResult CommentInput, Widget))
commentForm mIssueId =
    renderBootstrap3
        (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10))
        do
            issue <-
                areq
                    hiddenField
                    (bfs ("" :: Text)){fsName = Just "issue"}
                    mIssueId
            message <-
                unTextarea <$>
                areq
                    textareaField
                    (bfs ("Comment" :: Text)){fsName = Just "message"}
                    Nothing
            pure CommentInput{issue, message, requestUsers = Set.empty}
