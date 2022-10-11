{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Model.Vote (
    dbRecord,
    dbUpdateIssueApproval,
    record,
    updateIssueApproval,
) where

import Import hiding (Value)

import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Database.Esqueleto.Experimental (Value (Value), from, just, leftJoin, on,
                                        select, table, val, where_, (:&) ((:&)),
                                        (==.), (?.), (^.))
import Database.Persist (insert_, selectList, update, (=.))
import Database.Persist qualified as Persist
import Yesod.Persist (runDB)

import Genesis (mtlFund)
import Model (Comment (Comment),
              EntityField (Issue_approval, StellarSigner_key, StellarSigner_target, StellarSigner_weight, UserId, User_stellarAddress, Vote_choice, Vote_issue),
              Issue (Issue), IssueId, StellarSigner, User, UserId, Vote (Vote))
import Model qualified

-- | Create a vote or abrogate existing
record :: HasCallStack => IssueId -> Choice -> Handler ()
record issue choice = do
    user <- requireAuthId
    runDB $ dbRecord user issue choice

dbRecord ::
    (HasCallStack, MonadUnliftIO m) =>
    UserId -> IssueId -> Choice -> SqlPersistT m ()
dbRecord user issue choice = do
    addCallStack $ upsert_ Vote{user, issue, choice} [Vote_choice =. choice]
    now <- liftIO getCurrentTime
    addCallStack $
        insert_
            Comment
                { author            = user
                , created           = now
                , eventDelivered    = False
                , message           = mempty
                , parent            = Nothing
                , issue
                , type_ =
                    case choice of
                        Approve -> CommentApprove
                        Reject  -> CommentReject
                }
    addCallStack $ dbUpdateIssueApproval issue Nothing

updateIssueApproval ::
    HasCallStack =>
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    Handler ()
updateIssueApproval issueId = runDB . dbUpdateIssueApproval issueId

dbUpdateIssueApproval ::
    (HasCallStack, MonadUnliftIO m) =>
    IssueId ->
    -- | If the issue value is given it will be checked for the need of update.
    Maybe Issue ->
    SqlPersistT m ()
dbUpdateIssueApproval issueId mIssue = do
    weights :: [(Int, Maybe UserId)] <-
        addCallStack $
            select do
                signer :& user <-
                    from $
                        table @StellarSigner `leftJoin` table @User
                        `on` \(signer :& user) ->
                            just (signer ^. StellarSigner_key)
                            ==. user ?. User_stellarAddress
                where_ $ signer ^. StellarSigner_target ==. val mtlFund
                pure (signer ^. StellarSigner_weight, user ?. UserId)
            <&> coerce
    let totalSignersWeight = sum $ map fst weights
        userWeights =
            Map.fromList
                [(userId, weight) | (weight, Just userId) <- weights]
    -- TODO(cblp, 2022-02-20) cache weight selection for mass issue update

    approves <-
        addCallStack $
        selectList
            [Vote_choice Persist.==. Approve, Vote_issue Persist.==. issueId]
            []
    let approvers = map (\(Entity _ Vote{user}) -> user) approves

    let totalApproveWeight =
            sum [findWithDefault 0 userId userWeights | userId <- approvers]
    let approval
            | 0 <- totalSignersWeight = 0
            | otherwise = totalApproveWeight ./. totalSignersWeight
    case mIssue of
        Just Issue{approval = oldApproval} | approval == oldApproval -> pure ()
        _ -> addCallStack $ update issueId [Issue_approval =. approval]

(./.) :: (Integral a, Integral b) => a -> b -> Double
a ./. b = fromIntegral a / fromIntegral b
