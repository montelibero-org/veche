{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Comment (
    Comment (..),
    CommentId,
    CommentInput (..),
    CommentMaterialized (..),
    addText,
    updateIssueCommentNum,
) where

import Import.NoFoundation

import Database.Persist (count, getBy, getJustEntity, insert, insertMany_,
                         update, updateWhere, (<-.), (=.), (==.))
import Database.Persist.Sql (SqlBackend)
import Yesod.Persist (YesodPersist, YesodPersistBackend, get404, runDB)

import Genesis (mtlAsset, mtlFund)
import Model (Comment (Comment), CommentId,
              EntityField (Comment_issue, Comment_type, Issue_commentNum, RequestId, Request_fulfilled, Request_issue, Request_user),
              Issue (Issue), IssueId, Request (Request), RequestId,
              Unique (UniqueHolder, UniqueSigner), User (User), UserId)
import Model qualified

data CommentInput = CommentInput
    { issue        :: IssueId
    , message      :: Text
    , requestUsers :: Set UserId
    , provideInfo  :: Set RequestId
    , parent       :: Maybe CommentId
    }
    deriving Show

data CommentMaterialized = CommentMaterialized
    { id             :: CommentId
    , comment        :: Comment
    , author         :: Entity User
    , requestedUsers :: [User]
    }
    deriving (Show)

addText ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    Entity User -> CommentInput -> HandlerFor app CommentId
addText (Entity author User{stellarAddress}) commentInput = do
    now <- liftIO getCurrentTime
    let comment =
            Comment
                { author
                , created           = now
                , eventDelivered    = False
                , issue
                , message
                , parent
                , type_             = CommentText
                }
    runDB do
        Issue{forum} <- get404 issue
        forumE <- getJustEntity forum
        mSigner <- getBy $ UniqueSigner mtlFund stellarAddress
        mHolder <- getBy $ UniqueHolder mtlAsset stellarAddress
        let mSignerId = entityKey <$> mSigner
            mHolderId = entityKey <$> mHolder
        requireAuthz $ AddForumIssueComment forumE (mSignerId, mHolderId)
        commentId <- insert comment
        insertMany_
            [ makeRequest commentId now user
            | user <- toList requestUsers
            ]
        unsafeUpdateIssueCommentNum issue Nothing
        updateWhere
            [ RequestId <-. toList provideInfo
            -- following filters present only for security
            , Request_user  ==. author
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
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    HandlerFor app ()
updateIssueCommentNum issueId = runDB . unsafeUpdateIssueCommentNum issueId

unsafeUpdateIssueCommentNum ::
    MonadIO m =>
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    SqlPersistT m ()
unsafeUpdateIssueCommentNum issueId mIssue = do
    commentNum <-
        count [Comment_issue ==. issueId, Comment_type ==. CommentText]
    case mIssue of
        Just Issue{commentNum = oldCommentNum}
            | commentNum == oldCommentNum -> pure ()
        _ -> update issueId [Issue_commentNum =. commentNum]
