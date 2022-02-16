{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handler.Comment
    ( postCommentR
    ) where

import Import

import Text.Blaze.Html.Renderer.Text (renderHtml)

import Genesis (mtlFund)
import Templates.Comment (commentWidget)
import Types.Comment (CommentMaterialized (..))

data CommentRequest = CommentRequest{message :: Text, issue :: IssueId}
    deriving (FromJSON, Generic)

postCommentR :: Handler Value
postCommentR = do
    -- auth
    (authorId, author@User{userStellarAddress}) <- requireAuthPair

    -- input
    CommentRequest{message, issue} <- requireCheckJsonBody
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
    runDB do
        Entity signerId _ <- getBy403 $ UniqueSigner mtlFund userStellarAddress
        requireAuthz $ AddIssueComment signerId
        insert_ comment

    returnJson $ renderHtml $ commentWidget CommentMaterialized{author, comment}
