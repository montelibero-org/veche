{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Comment (addText, updateIssueCommentNum) where

import Import

import Genesis (mtlFund)
import Templates.Comment (CommentInput (..))

addText :: Entity User -> CommentInput -> Handler CommentId
addText
    (Entity userId User{userStellarAddress})
    CommentInput{issue, message, requestUsers, provideInfo} = do
        now <- liftIO getCurrentTime
        let comment =
                Comment
                    { commentAuthor  = userId
                    , commentCreated = now
                    , commentIssue   = issue
                    , commentMessage = message
                    , commentParent  = Nothing
                    , commentType    = CommentText
                    }
        runDB do
            Entity signerId _ <-
                getBy403 $ UniqueMember mtlFund userStellarAddress
            requireAuthz $ AddIssueComment signerId
            commentId <- insert comment
            insertMany_
                [ Request
                    { requestComment   = commentId
                    , requestFulfilled = False
                    , requestIssue     = issue
                    , requestUser
                    }
                | requestUser <- toList requestUsers
                ]
            updateIssueCommentNum issue Nothing
            updateWhere
                [ RequestId <-. toList provideInfo
                -- following filters present only for security
                , RequestUser ==. userId
                , RequestIssue ==. issue
                ]
                [RequestFulfilled =. True]
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
