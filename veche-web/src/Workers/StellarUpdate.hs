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
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml qualified as Yaml
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- project
import Stellar.Horizon.Client (Account (Account), Asset (Asset),
                               Memo (MemoText),
                               Operation (OperationChangeTrust, OperationCreateAccount, OperationCreateClaimableBalance, OperationPayment),
                               Transaction (Transaction),
                               TransactionOnChain (TransactionOnChain),
                               getAccount, getAccountTransactionsList,
                               getAccountsList)
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
    let (extra, active) =
            correctEscrow $ partitionEithers $ concatMap makeEscrows txs
    Aeson.encodeFile appEscrowActiveFile active
    Yaml.encodeFile  appEscrowExtraFile  extra
    atomicWriteIORef appEscrowActive $ Escrow.buildIndex active
  where
    App{appEscrowActive, appSettings} = app
    AppSettings{appEscrowActiveFile, appEscrowExtraFile} = appSettings

correctEscrow ::
    ([TransactionOnChain], [Escrow]) -> ([TransactionOnChain], [Escrow])
correctEscrow (extra, active) =
    ( filter (\TransactionOnChain{id} -> id `notElem` outputs) extra
    , filter (\Escrow{txId} -> txId `notMember` escrowCorrections) active
    )
  where
    outputs =
        Set.fromList
            [output | EscrowCorrection{output} <- toList escrowCorrections]

parseEscrowIssueIdFromMemo :: Text -> Maybe IssueId
parseEscrowIssueIdFromMemo memo = do
    issueIdText <- stripPrefix "E" memo
    fromPathPiece issueIdText

makeEscrows :: TransactionOnChain -> [Either TransactionOnChain Escrow]
makeEscrows toc@TransactionOnChain{id = txId, time, tx} = do
    operation <- operations
    let saveExtra =
            pure $ Left toc{Stellar.tx = tx{Stellar.operations = [operation]}}
    case operation of
        Right OperationChangeTrust -> []
        Right OperationCreateAccount -> []
        Right OperationCreateClaimableBalance -> []
        Right OperationPayment{amount, asset, destination, source}
            | destination == escrowAddress
            , MemoText memoText <- memo -> do
                let sponsor = fromMaybe txSource source
                issueId <- toList $ parseEscrowIssueIdFromMemo memoText
                pure $ Right Escrow{amount, asset, issueId, sponsor, time, txId}
        Right OperationPayment{destination, source}
            | fromMaybe txSource source /= escrowAddress
            , destination /= escrowAddress ->
                []
        _ -> saveExtra
  where
    Transaction{memo, operations, source = txSource} = tx
