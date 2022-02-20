{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Comment (addText) where

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
        updateCommentNum issue
    pure comment

updateCommentNum :: IssueId -> SqlPersistT Handler ()
updateCommentNum issueId = do
    commentNum <-
        count [CommentIssue ==. issueId, CommentType ==. CommentText]
    update issueId [IssueCommentNum =. commentNum]
