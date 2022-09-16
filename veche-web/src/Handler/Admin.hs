{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Admin (getAdminUpdateDatabaseR) where

import Import

import Model.Comment qualified as Comment
import Model.Issue qualified as Issue
import Model.Vote qualified as Vote

getAdminUpdateDatabaseR :: Handler TypedContent
getAdminUpdateDatabaseR = do
    respondSource "text/plain" do
        issues <- lift Issue.selectAll
        sendChunkText $
            "Updating issues "
            <> intercalate ", " (map (toPathPiece . entityKey) issues) <> "\n"
        for_ issues \(Entity issueId issue) -> do
            sendChunkText $ "Updating issue " <> toPathPiece issueId <> "\n"
            lift do
                Comment.updateIssueCommentNum issueId $ Just issue
                Vote.updateIssueApproval      issueId $ Just issue
        sendChunkText "Updated all issues\n"
