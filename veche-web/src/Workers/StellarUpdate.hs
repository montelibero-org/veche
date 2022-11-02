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
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- project
import Stellar.Horizon.Client (Account (Account), Asset (Asset),
                               Memo (MemoText), Operation (OperationPayment),
                               Transaction (Transaction), getAccount,
                               getAccountTransactionsList, getAccountsList)
import Stellar.Horizon.Client qualified as Stellar
import Stellar.Horizon.DTO (Balance (Balance), SignerType (Ed25519PublicKey))
import Stellar.Horizon.DTO qualified

-- component
import Genesis (escrowAddress, mtlAsset, mtlFund)
import Model.Escrow (Escrow (Escrow))
import Model.Escrow qualified as Escrow
import Model.Issue (IssueId)
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified as StellarHolder
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified as StellarSigner

stellarDataUpdater :: App -> IO ()
stellarDataUpdater app = do
    let clientEnv = mkClientEnv appHttpManager appStellarHorizon
    forever do
        do  putStrLn "stellarDataUpdater: Updating Escrow"
            updateEscrow app clientEnv
            putStrLn "stellarDataUpdater: Updated Escrow"
        randomDelay
        do  putStrLn "stellarDataUpdater: Updating MTL signers"
            n <- updateSignersCache clientEnv appConnPool mtlFund
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL signers"
        randomDelay
        do  putStrLn "stellarDataUpdater: Updating MTL holders"
            n <- updateHoldersCache clientEnv appConnPool mtlAsset
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL holders"
        randomDelay
  where
    App{appConnPool, appHttpManager, appStellarHorizon} = app
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
    holders <- runClientM' clientEnv $ getAccountsList asset
    let actual =
            Map.fromList
                [ (account_id, realToFrac amount)
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

getAmount :: Asset -> [Balance] -> Scientific
getAmount Asset{code, issuer} balances =
    fromMaybe 0 $
    asum
        [ readMaybe @Scientific $ Text.unpack balance
        | Balance
                { balance
                , asset_code
                , asset_issuer
                } <-
            balances
        , maybe isNative (== code) asset_code
        , asset_issuer == (Stellar.Address <$> issuer)
        ]
  where
    isNative = isNothing issuer

updateEscrow :: App -> ClientEnv -> IO ()
updateEscrow app clientEnv = do
    txs <- runClientM' clientEnv $ getAccountTransactionsList escrowAddress
    let escrows = concatMap makeEscrows txs
    atomicWriteIORef appEscrowsActive $ Escrow.buildIndex escrows
    Aeson.encodeFile appEscrowsActiveFile escrows
  where
    App{appEscrowsActive, appSettings} = app
    AppSettings{appEscrowsActiveFile} = appSettings

parseEscrowIssueIdFromMemo :: Text -> Maybe IssueId
parseEscrowIssueIdFromMemo memo = do
    issueIdText <- stripPrefix "E" memo
    fromPathPiece issueIdText

makeEscrows :: Transaction -> [Escrow]
makeEscrows Transaction{id, memo, operations, source = txSource} =
    [ Escrow{amount, asset, issueId, sponsor, txId = id}
    | MemoText memoText <- [memo]
    , issueId <- toList $ parseEscrowIssueIdFromMemo memoText
    , OperationPayment{amount, asset, destination, source = opSource} <-
        operations
    , destination == escrowAddress
    , let sponsor = fromMaybe txSource opSource
    ]
