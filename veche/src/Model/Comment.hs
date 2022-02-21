{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Comment (addText, updateIssueCommentNum) where

import Import

import Genesis (mtlFund)
import Templates.Comment (CommentInput (..))

addText :: Entity User -> CommentInput -> Handler CommentId
addText
    (Entity userId User{userStellarAddress})
    CommentInput{issue, message, requestUsers} = do
        now <- liftIO getCurrentTime
        let comment =
                Comment
                    { commentAuthor  = userId
                    , commentCreated = now
                    , commentMessage = message
                    , commentParent  = Nothing
                    , commentIssue   = issue
                    , commentType    = CommentText
                    }
        runDB do
            Entity signerId _ <-
                getBy403 $ UniqueMember mtlFund userStellarAddress
            requireAuthz $ AddIssueComment signerId
            commentId <- insert comment
            insertMany_
                [ Request
                    { requestUser
                    , requestComment   = commentId
                    , requestFulfilled = False
                    }
                | requestUser <- toList requestUsers
                ]
            updateIssueCommentNum issue Nothing
            pure commentId

updateIssueCommentNum ::
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    SqlPersistT Handler ()
updateIssueCommentNum issueId mIssue = do
    commentNum <- count [CommentIssue ==. issueId, CommentType ==. CommentText]
    case mIssue of
        Just Issue{issueCommentNum = oldCommentNum}
            | commentNum == oldCommentNum -> pure ()
        _ -> update issueId [IssueCommentNum =. commentNum]
