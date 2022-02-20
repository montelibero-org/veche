{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Admin (getAdminUpdateDatabaseR) where

import Import

import Model.Comment qualified as Comment
import Model.Vote qualified as Vote

getAdminUpdateDatabaseR :: Handler TypedContent
getAdminUpdateDatabaseR = do
    _ <- requireAuth
    respondSource "text/plain" do
        issues <- lift $ runDB $ selectKeysList [] []
        sendChunkText $
            "Updating issues " <> intercalate ", " (map toPathPiece issues)
            <> "\n"
        for_ issues \issueId -> do
            sendChunkText $ "Updating issue " <> toPathPiece issueId <> "\n"
            lift $ runDB do
                Comment.updateIssueCommentNum issueId
                Vote.updateIssueApproval issueId
        sendChunkText "Updated all issues\n"
