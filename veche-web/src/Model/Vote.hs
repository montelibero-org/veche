{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Model.Vote (dbUpdateIssueApproval, record, updateIssueApproval) where

import Import

import Data.Map.Strict qualified as Map
import Database.Persist (insert_, selectList, toPersistValue, update, (=.),
                         (==.))
import Database.Persist.Sql (Single, rawSql, unSingle)
import Yesod.Persist (runDB)

import Genesis (mtlFund)

-- | Create a vote or abrogate existing
record :: IssueId -> Choice -> Handler ()
record issueId choice = do
    now <- liftIO getCurrentTime
    (user, User{userStellarAddress}) <- requireAuthPair
    runDB do
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ AddVote signerId
        upsert_
            Vote{voteUser = user, voteIssue = issueId, voteChoice = choice}
            [VoteChoice =. choice]
        insert_
            Comment
                { commentAuthor     = user
                , commentCreated    = now
                , commentMessage    = mempty
                , commentParent     = Nothing
                , commentIssue      = issueId
                , commentType       =
                    case choice of
                        Approve -> CommentApprove
                        Reject  -> CommentReject
                }
        dbUpdateIssueApproval issueId Nothing

updateIssueApproval ::
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    Handler ()
updateIssueApproval issueId = runDB . dbUpdateIssueApproval issueId

dbUpdateIssueApproval ::
    MonadIO m =>
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    SqlPersistT m ()
dbUpdateIssueApproval issueId mIssue = do
    weights :: [(Int, Maybe UserId)] <-
        rawSql
            @(Single Int, Maybe UserId)
            "SELECT stellar_signer.weight, user.id\
            \ FROM stellar_signer LEFT JOIN user\
                \ ON stellar_signer.key = user.stellar_address\
            \ WHERE stellar_signer.target = ?"
            [toPersistValue mtlFund]
        <&> map (first unSingle)
    let totalSignersWeight = sum $ map fst weights
        userWeights =
            Map.fromList
                [(userId, weight) | (weight, Just userId) <- weights]
    -- TODO(cblp, 2022-02-20) cache weight selection for mass issue update

    approves <- selectList [VoteChoice ==. Approve, VoteIssue ==. issueId] []
    let approvers = map (voteUser . entityVal) approves

    let totalApproveWeights =
            sum [findWithDefault 0 userId userWeights | userId <- approvers]
    let approval =
            fromIntegral totalApproveWeights / fromIntegral totalSignersWeight
    case mIssue of
        Just Issue{issueApproval = oldApproval} | approval == oldApproval ->
            pure ()
        _ -> update issueId [IssueApproval =. approval]
