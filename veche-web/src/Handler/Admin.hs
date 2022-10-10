{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Admin (getAdminUpdateDatabaseR) where

import Import

import Database.Persist (selectList)
import Yesod.Persist (runDB)

import Model.Comment qualified as Comment
import Model.Vote qualified as Vote

getAdminUpdateDatabaseR :: Handler TypedContent
getAdminUpdateDatabaseR =
    respondSource "text/plain" do
        issues <- lift $ runDB $ selectList [] []
        sendChunkText $
            "Updating issues "
            <> intercalate ", " (map (toPathPiece . entityKey) issues) <> "\n"
        for_ issues \(Entity issueId issueVal) -> do
            sendChunkText $ "Updating issue " <> toPathPiece issueId <> "\n"
            lift do
                Comment.updateIssueCommentNum issueId $ Just issueVal
                Vote.updateIssueApproval      issueId $ Just issueVal
        sendChunkText "✅ Updated all issues\n"
