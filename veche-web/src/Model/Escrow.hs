{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Escrow (
    EscrowFile,
    EscrowStat (..),
    buildEscrow,
    getIssueBalance,
) where

-- prelude
import Import

-- global
import Control.Monad.Writer (execWriter, tell)
import Data.Map.Strict qualified as Map

-- project
import Stellar.Simple (Asset, Memo (MemoText), Operation (OperationPayment),
                       Transaction (Transaction),
                       TransactionOnChain (TransactionOnChain))
import Stellar.Simple qualified

-- component
import Genesis (escrowAddress, mtlKeyRateAccount)
import Model (Corrections (Corrections), EscrowFile, EscrowStat (..), IssueId,
              TransactionsOnChain)
import Model qualified

parseEscrowIssueIdFromMemo :: Text -> Maybe IssueId
parseEscrowIssueIdFromMemo memo = do
    issueIdText <- stripPrefix "E" memo
    fromPathPiece issueIdText

buildEscrow :: Corrections -> TransactionsOnChain -> EscrowStat
buildEscrow Corrections{exclude, include} txs =
    cleanZeroBalances $
    execWriter $
    for_ txs \toc@TransactionOnChain{id, tx} -> do
        let Transaction{memo, operations, source} = tx
        if  | Just issueId <- Map.lookup id include ->
                handleOperations source issueId operations
            | id `elem` exclude || source == mtlKeyRateAccount ->
                tellExcluded toc
            | MemoText memoText <- memo
            , Just issueId <- parseEscrowIssueIdFromMemo memoText ->
                    handleOperations source issueId operations
            | otherwise -> tellUnknownTx toc
  where

    cleanZeroBalances stat@EscrowStat{balances} =
        stat{ balances =
                Map.filter (not . null) $ Map.filter (/= 0) <$> balances
            }

    handleOperations txSource issueId operations =
        for_ operations \case
            Right OperationPayment
                    {amount, asset, destination, source = opSource}
                | destination == escrowAddress ->
                    tellBalance issueId asset amount
                | source == escrowAddress ->
                    tellBalance issueId asset (-amount)
                where
                    source = fromMaybe txSource opSource
            Right op -> tellUnknownOp op
            Left _ -> pure ()

    tellBalance issueId asset amount =
        tell
            mempty
            {balances = Map.singleton issueId $ Map.singleton asset amount}

    tellExcluded tx = tell mempty{excluded = [tx]}

    tellUnknownOp op = tell mempty{unknownOps = [op]}

    tellUnknownTx tx = tell mempty{unknownTxs = [tx]}

-- | Get active (unspent) escrows for the issue
getIssueBalance :: IssueId -> Handler (Map Asset Scientific)
getIssueBalance issue = do
    App{appEscrow} <- getYesod
    EscrowStat{balances} <- readIORef appEscrow
    pure $ Map.findWithDefault mempty issue balances
