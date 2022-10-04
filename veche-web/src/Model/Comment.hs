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
addText (Entity userId User{stellarAddress}) commentInput = do
    now <- liftIO getCurrentTime
    let comment =
            Comment
                { author            = userId
                , created           = now
                , eventDelivered    = False
                , issue
                , message
                , parent
                , type_             = CommentText
                }
    runDB do
        Entity holderId _ <- getBy403 $ UniqueHolder mtlAsset stellarAddress
        requireAuthz $ AddIssueComment holderId
        commentId <- insert comment
        insertMany_
            [ makeRequest commentId now user
            | user <- toList requestUsers
            ]
        unsafeUpdateIssueCommentNum issue Nothing
        updateWhere
            [ RequestId <-. toList provideInfo
            -- following filters present only for security
            , Request_user  ==. userId
            , Request_issue ==. issue
            ]
            [Request_fulfilled =. True]
        pure commentId
  where
    CommentInput{issue, message, requestUsers, provideInfo, parent} =
        commentInput
    makeRequest comment created user =
        Request
            { comment
            , created
            , eventDelivered    = False
            , fulfilled         = False
            , issue
            , user
            }

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
    commentNum <-
        count [Comment_issue ==. issueId, Comment_type ==. CommentText]
    case mIssue of
        Just Issue{commentNum = oldCommentNum}
            | commentNum == oldCommentNum -> pure ()
        _ -> update issueId [Issue_commentNum =. commentNum]
