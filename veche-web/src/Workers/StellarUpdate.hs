{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Workers.StellarUpdate (stellarDataUpdater) where

import Import

-- global
import Control.Concurrent (threadDelay)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ClientEnv, ClientM, mkClientEnv, runClientM)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- project
import Stellar.Horizon.Client (getAccount, getAllAccounts)
import Stellar.Horizon.Types (Account (Account), Asset (Asset),
                              Balance (Balance), SignerType (Ed25519PublicKey))
import Stellar.Horizon.Types qualified as Stellar

-- component
import Genesis (mtlAsset, mtlFund)
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified as StellarHolder
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified as StellarSigner

stellarDataUpdater :: BaseUrl -> ConnectionPool -> Manager -> IO ()
stellarDataUpdater baseUrl connPool manager = do
    let clientEnv = mkClientEnv manager baseUrl
    forever do
        do  putStrLn "stellarDataUpdater: Updating MTL signers"
            n <- updateSignersCache clientEnv connPool mtlFund
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL signers"
        randomDelay
        do  putStrLn "stellarDataUpdater: Updating MTL holders"
            n <- updateHoldersCache clientEnv connPool mtlAsset
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL holders"
        randomDelay
  where
    randomDelay = do
        delayMinutes <- randomRIO (1, 10)
        threadDelay $ delayMinutes * 60 * 1_000_000

updateSignersCache ::
    ClientEnv ->
    ConnectionPool ->
    StellarMultiSigAddress ->
    -- | Actual number of signers
    IO Int
updateSignersCache clientEnv connPool target = do
    Account{signers} <- runClientM' clientEnv $ getAccount address
    let actual =
            Map.fromList
                [ (Stellar.Address key, weight)
                | Stellar.Signer{key, weight, type_ = Ed25519PublicKey} <-
                    signers
                , weight > 0
                ]
    (`runSqlPool` connPool) do
        cached' <- StellarSigner.dbGetAll target
        let cached =
                Map.fromList
                    [ (key, weight)
                    | Entity _ StellarSigner{key, weight} <- cached'
                    ]
        handleDeleted $ Map.keys $ cached \\ actual
        handleAdded $ actual \\ cached
        handleModified
            [ (key, weightActual)
            | (key, (weightCached, weightActual)) <-
                Map.assocs $ Map.intersectionWith (,) cached actual
            , weightCached /= weightActual
            ]
        when (cached /= actual) Issue.dbUpdateAllIssueApprovals
    pure $ length actual
  where

    StellarMultiSigAddress address = target

    handleDeleted = traverse_ $ StellarSigner.dbDelete target

    handleAdded = StellarSigner.dbInsertMany target . Map.assocs

    handleModified = traverse_ $ uncurry $ StellarSigner.dbSetWeight target

updateHoldersCache ::
    ClientEnv ->
    ConnectionPool ->
    Asset ->
    -- | Actual number of holders
    IO Int
updateHoldersCache clientEnv connPool asset = do
    holders <- runClientM' clientEnv $ getAllAccounts asset
    let actual =
            Map.fromList
                [ (account_id, amount)
                | Account{account_id, balances} <- holders
                , let amount = getAmount asset balances
                , amount > 0
                ]
    (`runSqlPool` connPool) do
        cached' <- StellarHolder.dbGetAll asset
        let cached =
                Map.fromList
                    [ (key, amount)
                    | Entity _ StellarHolder{key, amount} <- cached'
                    ]
        handleDeleted $ Map.keys $ cached \\ actual
        handleAdded $ actual \\ cached
        handleModified
            [ (key, shareActual)
            | (key, (shareCached, shareActual)) <-
                Map.assocs $ Map.intersectionWith (,) cached actual
            , shareCached /= shareActual
            ]
        when (cached /= actual) Issue.dbUpdateAllIssueApprovals
    pure $ length actual
  where
    handleDeleted = traverse_ $ StellarHolder.dbDelete asset
    handleAdded = StellarHolder.dbInsertMany asset . Map.assocs
    handleModified = traverse_ $ uncurry $ StellarHolder.dbSetShare asset

runClientM' :: ClientEnv -> ClientM a -> IO a
runClientM' clientEnv action =
    runClientM action clientEnv >>= either throwIO pure

getAmount :: Asset -> [Balance] -> Decimal
getAmount (Asset asset) balances =
    fromMaybe 0 $
    asum
        [ readMaybe @Decimal $ Text.unpack balance
        | Balance
                { balance
                , asset_code = Just code
                , asset_issuer = Just (Stellar.Address issuer)
                } <-
            balances
        , asset == code <> ":" <> issuer
        ]
