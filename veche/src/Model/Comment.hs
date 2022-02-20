{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Comment (addText, updateIssueCommentNum) where

import Import

import Genesis (mtlFund)

addText :: Entity User -> IssueId -> Text -> Handler Comment
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
        insert_ comment
        updateIssueCommentNum issue
    pure comment

updateIssueCommentNum :: IssueId -> SqlPersistT Handler ()
updateIssueCommentNum issueId = do
    commentNum <-
        count [CommentIssue ==. issueId, CommentType ==. CommentText]
    update issueId [IssueCommentNum =. commentNum]
