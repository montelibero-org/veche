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

import Genesis (mtlFund)
import Model.Forum (Forum (Forum), Key (ForumKey))
import Model.Forum qualified
import Model.Issue (Issue (Issue), Key (IssueKey))
import Model.Issue qualified as Issue
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified
import Model.Types (AccessLevel (AccessLevelUninvolved),
                    Choice (Approve, Reject))
import Model.User (User (User))
import Model.User qualified
import Model.Vote qualified as Vote

spec :: Spec
spec =
    withApp $
        it "calculates approval" do
            userApprover <- createUser "approver" Nothing
            userRejector <- createUser "rejector" Nothing

            prepare $ userApprover :| [userRejector]
            recordVote userApprover Approve
            recordVote userRejector Reject

            Issue{approval} <- runDB $ get404 issueId
            approval === 0.5
  where
    forumId = ForumKey "POINT"
    issueId = IssueKey 1

    prepare users@(user :| _) = do
        authenticateAs user

        runDB do
            -- create the forum
            insertKey
                forumId
                Forum
                    { title                 = "Point forum"
                    , accessIssueRead       = AccessLevelUninvolved
                    , accessIssueWrite      = AccessLevelUninvolved
                    , accessIssueComment    = AccessLevelUninvolved
                    }
            for_ users \(Entity _ User{stellarAddress}) ->
                insert_
                    StellarSigner
                        {target = mtlFund, key = stellarAddress, weight = 1}

        get $ ForumIssueNewR forumId -- get CSRF token
        statusIs 200

        request do
            setMethod "POST"
            setUrl $ ForumIssuesR forumId
            addRequestHeader ("Accept", "text/plain")
            addTokenFromCookie
            addPostParam "title" "Flight ate"
            addPostParam "body" "Slide weight graph. Such bowl baby."
            addPostParam "poll" "1"
        statusIs 303

    recordVote (Entity userId _) choice =
        runDB $ Vote.dbRecord userId issueId choice
