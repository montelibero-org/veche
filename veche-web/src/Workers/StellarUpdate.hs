{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Workers.StellarUpdate (stellarDataUpdater) where

-- prelude
import Import

-- global
import Control.Concurrent (threadDelay)
import Data.Aeson qualified as Aeson
import Data.Function (on)
import Data.List (cycle)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.Persist.Sql (runSqlPool)
import Network.HTTP.Types (notFound404)
import Servant.Client (
    ClientError (FailureResponse),
    ClientM,
    ResponseF (Response),
    mkClientEnv,
    responseStatusCode,
    runClientM,
 )
import System.Random (randomRIO)
import Yesod.Core (messageLoggerSource)

-- project
import Stellar.Horizon.Client (
    Account (Account),
    Asset (Asset),
    Operation (OperationPayment),
    Transaction (Transaction),
    TransactionOnChain (TransactionOnChain),
    getAccount,
    getAccountTransactionsList,
    getAccountsList,
 )
import Stellar.Horizon.Client qualified as Stellar
import Stellar.Horizon.DTO (Balance (Balance), SignerType (Ed25519PublicKey))
import Stellar.Horizon.DTO qualified

-- component
import Genesis (
    escrowAddress,
    fcmAsset,
    mtlAsset,
    mtlFund,
    showKnownAsset,
    vecheAsset,
 )
import Model.Escrow qualified as Escrow
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified as StellarHolder
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified as StellarSigner

stellarDataUpdater :: App -> IO ()
stellarDataUpdater app@App{appLogger} =
    runLogger
        $ for_ (cycle actions) \action -> do
            action
            randomDelay
  where
    runLogger = (`runLoggingT` messageLoggerSource app appLogger)

    actions =
        [ do
            $logInfo "stellarDataUpdater: Updating Escrow"
            liftIO $ updateEscrow app
            $logInfo "stellarDataUpdater: Updated Escrow"
        , do
            $logInfo "stellarDataUpdater: Updating MTL signers"
            n <- liftIO $ updateSignersCache app
            $logInfo
                $ "stellarDataUpdater: Updated "
                <> tshow n
                <> " MTL signers"
        , updateHoldersCache' fcmAsset
        , updateHoldersCache' mtlAsset
        , updateHoldersCache' vecheAsset
        ]

    updateHoldersCache' asset = do
        $logInfo
            $ unwords
                [ "stellarDataUpdater: Updating"
                , showKnownAsset asset
                , "holders"
                ]
        n <- liftIO $ updateHoldersCache app asset
        $logInfo
            $ unwords
                [ "stellarDataUpdater: Updated"
                , tshow n
                , showKnownAsset asset
                , "holders"
                ]

    randomDelay =
        liftIO do
            delaySeconds <- randomRIO (1, 60)
            threadDelay $ delaySeconds * 1_000_000

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
    Asset ->
    -- | Actual number of holders
    IO Int
updateHoldersCache app@App{appConnPool} asset = do
    holders <- runHorizonClient app $ getAccountsList asset
    let actual =
            Map.fromList
                [ (account_id, amount)
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
    handleDeleted = traverse_ $ StellarHolder.dbDelete asset
    handleAdded = StellarHolder.dbInsertMany asset . Map.assocs
    handleModified = traverse_ $ uncurry $ StellarHolder.dbSetShare asset

runHorizonClient :: App -> ClientM a -> IO a
runHorizonClient App{appHttpManager, appStellarHorizon} action =
    runClientM action (mkClientEnv appHttpManager appStellarHorizon)
        >>= either throwIO pure

getAmount :: Asset -> [Balance] -> Scientific
getAmount Asset{code, issuer} balances =
    fromMaybe 0
        $ asum
            [ readMaybe @Scientific $ Text.unpack balance
            | Balance{balance, asset_code, asset_issuer} <- balances
            , asset_code == Just code
            , asset_issuer == issuer
            ]

updateEscrow :: App -> IO ()
updateEscrow app@App{appEscrow, appSettings = AppSettings{appEscrowFile}} = do
    txs <-
        runHorizonClient app (getAccountTransactionsList escrowAddress)
            `catch404` \_ -> pure []
    let newPayments = filterPaymentTxs txs
    oldPayments <-
        Aeson.eitherDecodeFileStrict appEscrowFile >>= either error pure
    let payments = mergePayments oldPayments newPayments
    Aeson.encodeFile appEscrowFile payments
    let escrow = Escrow.buildEscrow escrowCorrections payments
    atomicWriteIORef appEscrow escrow
  where
    catch404 =
        catchIf \case
            FailureResponse _ Response{responseStatusCode} ->
                responseStatusCode == notFound404
            _ -> False

    catchIf cond = catchJust $ guard . cond

    -- Take only escrow in/out payment ops from txs
    filterPaymentTxs txs =
        [ toc{Stellar.tx = tx{Stellar.operations = operations'}}
        | toc <- txs
        , let
            TransactionOnChain{tx} = toc
            Transaction{source = txSource, operations} = tx
            operations' = filterPaymentOps txSource operations
        , not $ null operations'
        ]

    -- Take only escrow in/out payment ops (from ops)
    filterPaymentOps txSource ops =
        [ op
        | op@(Right OperationPayment{source = opSource, destination}) <- ops
        , let source = fromMaybe txSource opSource
        , destination == escrowAddress || source == escrowAddress
        ]

    mergePayments ::
        [TransactionOnChain] -> [TransactionOnChain] -> [TransactionOnChain]
    mergePayments = go `on` sortWith (.time)
      where
        go [] news = news
        go olds [] = olds
        go (old : olds) (new : news) =
            case compare old.time new.time of
                GT -> new : go (old : olds) news
                EQ | old.id == new.id -> old : go olds news
                _ -> old : go olds (new : news)
