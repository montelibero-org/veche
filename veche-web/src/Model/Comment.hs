{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Comment (
    Comment (..),
    CommentId,
    CommentInput (..),
    CommentMaterialized (..),
    addText,
    updateIssueCommentNum,
) where

-- prelude
import Import

-- global
import Database.Persist (count, insert, insertMany_, update, updateWhere, (<-.),
                         (=.), (==.))
import Database.Persist.Sql (SqlBackend)
import Yesod.Persist (YesodPersist, YesodPersistBackend, get404, runDB)

-- component
import Model (Comment (Comment), CommentId, Issue (Issue), IssueId,
              Request (Request), RequestId, User, UserId)
import Model qualified
import Model.Forum qualified as Forum
import Model.User (maybeAuthzRoles)

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
    (AuthId App ~ UserId, AuthEntity App ~ User) =>
    CommentInput -> Handler CommentId
addText commentInput = do
    author <- requireAuthId
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
        forumE <- Forum.getJustEntity forum
        (_, roles) <- maybeAuthzRoles
        requireAuthz $ AddForumIssueComment forumE roles

        commentId <- insert comment
        insertMany_
            [ makeRequest commentId now user
            | user <- toList requestUsers
            ]
        unsafeUpdateIssueCommentNum issue Nothing
        updateWhere
            @_ @_ @Request
            [ #id <-. toList provideInfo
            -- following filters present only for security
            , #user  ==. author
            , #issue ==. issue
            ]
            [#fulfilled =. True]
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
        count @_ @_ @Comment [#issue ==. issueId, #type ==. CommentText]
    case mIssue of
        Just Issue{commentNum = oldCommentNum}
            | commentNum == oldCommentNum -> pure ()
        _ -> update issueId [#commentNum =. commentNum]
