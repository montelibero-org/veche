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

-- prelude
import Import

-- global
import Control.Concurrent (threadDelay)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.Persist.Sql (runSqlPool)
import Servant.Client (ClientM, mkClientEnv, runClientM)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- project
import Stellar.Horizon.Client (Account (Account), Asset (Asset),
                               Operation (OperationPayment),
                               Transaction (Transaction),
                               TransactionOnChain (TransactionOnChain),
                               getAccount, getAccountTransactionsList,
                               getAccountsList)
import Stellar.Horizon.Client qualified as Stellar
import Stellar.Horizon.DTO (Balance (Balance), SignerType (Ed25519PublicKey))
import Stellar.Horizon.DTO qualified

-- component
import Genesis (escrowAddress, mtlAsset, mtlFund)
import Model.Escrow qualified as Escrow
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified as StellarHolder
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified as StellarSigner

stellarDataUpdater :: App -> IO ()
stellarDataUpdater app =
    forever do
        do  putStrLn "stellarDataUpdater: Updating Escrow"
            updateEscrow app
            putStrLn "stellarDataUpdater: Updated Escrow"
        randomDelay
        do  putStrLn "stellarDataUpdater: Updating MTL signers"
            n <- updateSignersCache app
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL signers"
        randomDelay
        do  putStrLn "stellarDataUpdater: Updating MTL holders"
            n <- updateHoldersCache app
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL holders"
        randomDelay
  where
    randomDelay = do
        delayMinutes <- randomRIO (1, 10)
        threadDelay $ delayMinutes * 60 * 1_000_000

updateSignersCache ::
    App ->
    -- | Actual number of signers
    IO Int
updateSignersCache app@App{appConnPool} = do
    Account{signers} <- runHorizonClient app $ getAccount address
    let actual =
            Map.fromList
                [ (Stellar.Address key, weight)
                | Stellar.Signer{key, weight, type_ = Ed25519PublicKey} <-
                    signers
                , weight > 0
                ]
    (`runSqlPool` appConnPool) do
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

    target = mtlFund

    StellarMultiSigAddress address = target

    handleDeleted = traverse_ $ StellarSigner.dbDelete target

    handleAdded = StellarSigner.dbInsertMany target . Map.assocs

    handleModified = traverse_ $ uncurry $ StellarSigner.dbSetWeight target

updateHoldersCache ::
    App ->
    -- | Actual number of holders
    IO Int
updateHoldersCache app@App{appConnPool} = do
    holders <- runHorizonClient app $ getAccountsList asset
    let actual =
            Map.fromList
                [ (account_id, realToFrac amount)
                | Account{account_id, balances} <- holders
                , let amount = getAmount asset balances
                , amount > 0
                ]
    (`runSqlPool` appConnPool) do
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
    asset = mtlAsset
    handleDeleted = traverse_ $ StellarHolder.dbDelete asset
    handleAdded = StellarHolder.dbInsertMany asset . Map.assocs
    handleModified = traverse_ $ uncurry $ StellarHolder.dbSetShare asset

runHorizonClient :: App -> ClientM a -> IO a
runHorizonClient App{appHttpManager, appStellarHorizon} action =
    runClientM action (mkClientEnv appHttpManager appStellarHorizon)
    >>= either throwIO pure

getAmount :: Asset -> [Balance] -> Scientific
getAmount Asset{code, issuer} balances =
    fromMaybe 0 $
    asum
        [ readMaybe @Scientific $ Text.unpack balance
        | Balance{balance, asset_code, asset_issuer} <- balances
        , asset_code == Just code
        , asset_issuer == issuer
        ]

updateEscrow :: App -> IO ()
updateEscrow app@App{appEscrow, appSettings = AppSettings{appEscrowFile}} = do
    txs <- runHorizonClient app $ getAccountTransactionsList escrowAddress
    let payments = filterPaymentTxs txs
    Aeson.encodeFile appEscrowFile payments
    let escrow = Escrow.buildEscrow escrowCorrections payments
    atomicWriteIORef appEscrow escrow
  where

    filterPaymentTxs txs =
        [ toc{Stellar.tx = tx{Stellar.operations = operations'}}
        | toc <- txs
        , let
            TransactionOnChain{tx} = toc
            Transaction{source = txSource, operations} = tx
            operations' = filterPaymentOps txSource operations
        , not $ null operations'
        ]

    filterPaymentOps txSource ops =
        [ op
        | op@(Right OperationPayment{source = opSource, destination}) <- ops
        , let source = fromMaybe txSource opSource
        , destination == escrowAddress || source == escrowAddress
        ]
