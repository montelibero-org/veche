{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Vote (record, updateIssueApprovals) where

import Import

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Database.Persist.Sql (Single (..), rawSql)

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

updateIssueApprovals :: SqlPersistT Handler ()
updateIssueApprovals = do
    weights :: [(Single Int, Maybe UserId)] <-
        rawSql
            "SELECT stellar_signer.weight, user.id\
            \ FROM stellar_signer LEFT JOIN user\
                \ ON stellar_signer.key = user.stellar_address\
            \ WHERE stellar_signer.target = ?"
            [toPersistValue mtlFund]
    let sumWeight = fromIntegral $ sum $ map (unSingle . fst) weights
        userWeights =
            Map.fromList
                [(userId, weight) | (Single weight, Just userId) <- weights]

    approvesForAllIssues <- selectList [VoteChoice ==. Approve] []
    let approversByIssue =
            Map.fromListWith
                (<>)
                [ (voteIssue, Set.singleton voteUser)
                | Entity _ Vote{voteIssue, voteUser} <- approvesForAllIssues
                ]

    for_ (Map.assocs approversByIssue) \(issueId, approvers) -> do
        let sumApproveWeights =
                sum
                    [ Map.findWithDefault 0 userId userWeights
                    | userId <- toList approvers
                    ]
        let approval = fromIntegral sumApproveWeights / sumWeight
        update issueId [IssueApproval =. approval]
