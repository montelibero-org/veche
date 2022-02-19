{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Vote (record) where

import Import

import Genesis (mtlFund)

-- | Create a vote or abrogate existing
record :: Choice -> IssueId -> Handler ()
record choice issueId = do
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
