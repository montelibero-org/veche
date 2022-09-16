{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Comment (addText, updateIssueCommentNum) where

import Import

import Genesis (mtlAsset)
import Templates.Comment (CommentInput (CommentInput))
import Templates.Comment qualified

addText :: Entity User -> CommentInput -> Handler CommentId
addText (Entity userId User{userStellarAddress})
        CommentInput{issue, message, requestUsers, provideInfo, parent} = do
    now <- liftIO getCurrentTime
    let comment =
            Comment
                { commentAuthor  = userId
                , commentCreated = now
                , commentIssue   = issue
                , commentMessage = message
                , commentParent  = parent
                , commentType    = CommentText
                }
    runDB do
        Entity holderId _ <- getBy403 $ UniqueHolder mtlAsset userStellarAddress
        requireAuthz $ AddIssueComment holderId
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
        unsafeUpdateIssueCommentNum issue Nothing
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
    Handler ()
updateIssueCommentNum issueId = runDB . unsafeUpdateIssueCommentNum issueId

unsafeUpdateIssueCommentNum ::
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    SqlPersistT Handler ()
unsafeUpdateIssueCommentNum issueId mIssue = do
    commentNum <- count [CommentIssue ==. issueId, CommentType ==. CommentText]
    case mIssue of
        Just Issue{issueCommentNum = oldCommentNum}
            | commentNum == oldCommentNum -> pure ()
        _ -> update issueId [IssueCommentNum =. commentNum]
