{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Comment (addText, updateIssueCommentNum) where

import Import

import Genesis (mtlFund)

addText :: Entity User -> IssueId -> Text -> Handler (Entity Comment)
addText (Entity userId User{userStellarAddress}) issue message = do
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
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ AddIssueComment signerId
        commentId <- insert comment
        updateIssueCommentNum issue Nothing
        pure $ Entity commentId comment

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
