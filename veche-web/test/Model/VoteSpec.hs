{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.VoteSpec (spec) where

import TestImport

import Data.List.NonEmpty (NonEmpty ((:|)))
import Yesod.Persist (get404)

import Genesis (mtlAsset)
import Model.Forum (ForumId (ForumKey))
import Model.Issue (Issue (Issue), Key (IssueKey))
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified
import Model.Types (Choice (Approve, Reject), Poll (ByMtlAmount))
import Model.User (User (User))
import Model.User qualified
import Model.Vote qualified as Vote

spec :: Spec
spec =
    withApp $
        it "calculates approval" do
            userApprover <- createUser "approver" Nothing
            userRejector <- createUser "rejector" Nothing

            prepare $ (userApprover, 42) :| [(userRejector, 8)]
            recordVote userApprover Approve
            recordVote userRejector Reject

            Issue{approval, poll} <- runDB $ get404 issueId
            poll === Just ByMtlAmount
            approval === 0.84
  where
    forumId = ForumKey "MTL-HOLDERS"
    issueId = IssueKey 1

    prepare users@((user, _) :| _) = do
        authenticateAs user

        runDB $
            for_ users \(Entity _ User{stellarAddress}, amount) ->
                insert_
                    StellarHolder
                        {asset = mtlAsset, key = stellarAddress, amount}

        get $ ForumIssueNewR forumId -- get CSRF token
        statusIs 200

        request do
            setMethod "POST"
            setUrl $ ForumIssuesR forumId
            addRequestHeader ("Accept", "text/plain")
            addTokenFromCookie
            addPostParam "title" "Flight ate"
            addPostParam "body" "Slide weight graph. Such bowl baby."
            addPostParam "poll" "MTL"
        statusIs 303

    recordVote (Entity userId _) choice =
        runDB $ Vote.dbRecord userId issueId choice
