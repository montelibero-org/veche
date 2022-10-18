{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Model.Vote (
    dbRecord,
    dbUpdateIssueApproval,
    record,
    updateIssueApproval,
) where

import Import hiding (Value)

import Data.Map.Strict qualified as Map
import Database.Esqueleto.Experimental (from, just, leftJoin, on, select, table,
                                        unValue, val, where_, (:&) ((:&)),
                                        (==.), (?.), (^.))
import Database.Persist (getJustEntity, insert_, selectList, update, (=.))
import Database.Persist qualified as Persist
import Yesod.Persist (runDB)

import Genesis (mtlAsset, mtlFund)
import Model (Comment (Comment), Issue (Issue), IssueId, StellarHolder,
              StellarSigner, User, UserId, Vote (Vote))
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
    addCallStack $ upsert_ Vote{user, issue, choice} [#choice =. choice]
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
                        Abstain -> CommentAbstain
                }
    dbLoadAndUpdateIssueApproval issue

updateIssueApproval :: HasCallStack => Entity Issue -> Handler ()
updateIssueApproval = runDB . dbUpdateIssueApproval

dbLoadAndUpdateIssueApproval ::
    (HasCallStack, MonadUnliftIO m) => IssueId -> SqlPersistT m ()
dbLoadAndUpdateIssueApproval id = do
    issue <- getJustEntity id
    dbUpdateIssueApproval issue

dbUpdateIssueApproval ::
    (HasCallStack, MonadUnliftIO m) => Entity Issue -> SqlPersistT m ()
dbUpdateIssueApproval (Entity issueId Issue{approval = oldApproval, poll}) = do
    weights <- getWeights
    let sumWeight = sum $ map fst weights
        userWeights =
            Map.fromList
                [(userId, weight) | (weight, Just userId) <- weights]
    -- TODO(cblp, 2022-02-20) cache weight selection for mass issue update

    approves <-
        addCallStack $
        selectList [#choice Persist.==. Approve, #issue Persist.==. issueId] []
    let approvers = map (\(Entity _ Vote{user}) -> user) approves

    let approveWeight =
            sum [findWithDefault 0 userId userWeights | userId <- approvers]
    let approval
            | 0 <- sumWeight    = 0
            | otherwise         = approveWeight / sumWeight
    when (realToFrac approval /= oldApproval) $
        addCallStack $ update issueId [#approval =. realToFrac approval]
  where

    getWeights :: MonadUnliftIO m => SqlPersistT m [(Decimal, Maybe UserId)]
    getWeights =
        case poll of
            Nothing -> pure []
            Just BySignerWeight ->
                addCallStack $
                    select do
                        signer :& user <- from $
                            table @StellarSigner `leftJoin` table @User
                            `on` \(signer :& user) ->
                                just (signer ^. #key)
                                ==. user ?. #stellarAddress
                        where_ $ signer ^. #target ==. val mtlFund
                        pure (signer ^. #weight, user ?. #id)
                    <&> map (bimap (fromIntegral . unValue) unValue)
            Just ByMtlAmount ->
                addCallStack $
                    select do
                        holder :& user <- from $
                            table @StellarHolder `leftJoin` table @User
                            `on` \(holder :& user) ->
                                just (holder ^. #key)
                                ==. user ?. #stellarAddress
                        where_ $ holder ^. #asset ==. val mtlAsset
                        pure (holder ^. #amount, user ?. #id)
                    <&> map (bimap unValue unValue)
