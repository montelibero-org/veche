{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Comment (addText, updateIssueCommentNum) where

import Import

import Database.Persist (count, insert, insertMany_, update, updateWhere, (<-.),
                         (=.), (==.))
import Yesod.Persist (runDB)

import Genesis (mtlAsset)
import Templates.Comment (CommentInput (CommentInput))
import Templates.Comment qualified

addText :: Entity User -> CommentInput -> Handler CommentId
addText (Entity userId User{stellarAddress})
        CommentInput{issue, message, requestUsers, provideInfo, parent} = do
    now <- liftIO getCurrentTime
    let comment =
            Comment
                { author    = userId
                , created   = now
                , issue
                , message
                , parent
                , type_     = CommentText
                }
    runDB do
        Entity holderId _ <- getBy403 $ UniqueHolder mtlAsset stellarAddress
        requireAuthz $ AddIssueComment holderId
        commentId <- insert comment
        insertMany_
            [ Request{comment = commentId, fulfilled = False, issue, user}
            | user <- toList requestUsers
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
        Just Issue{commentNum = oldCommentNum}
            | commentNum == oldCommentNum -> pure ()
        _ -> update issueId [IssueCommentNum =. commentNum]
