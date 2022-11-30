{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Audit (
    getAuditEscrowR,
) where

{- HLINT ignore "Fuse on/on" -}

-- prelude
import Authorization
import Import hiding (Value)

-- global
import Data.Map.Strict qualified as Map
import Database.Persist (get)
import Yesod.Core (liftHandler)
import Yesod.Persist (runDB)

-- project
import Stellar.Simple (Asset, TransactionOnChain (TransactionOnChain))
import Stellar.Simple qualified

-- component
import Genesis (showKnownAsset)
import Model.Escrow (EscrowStat (EscrowStat))
import Model.Escrow qualified
import Model.Issue (Issue (Issue), IssueId)
import Model.Issue qualified
import Model.UserRole qualified as UserRole

getAuditEscrowR :: Handler Html
getAuditEscrowR = do
    roleE <- UserRole.get Audit ?|> permissionDenied ""
    requireAuthz $ AuditOp roleE

    App{appEscrow} <- getYesod

    EscrowStat{balances, unknownOps, unknownTxs} <-
        readIORef appEscrow
    defaultLayout $(widgetFile "audit/escrow")

balancesHead :: Widget
balancesHead =
    [whamlet|
        <th>issue
        <th>amount
    |]

balancesRow :: (IssueId, Map Asset Scientific) -> Widget
balancesRow (issueId, amounts) = do
    mIssue <- liftHandler $ runDB $ get issueId
    let issueIsClosed = any (\Issue{..} -> not open) mIssue
    [whamlet|
        <td>
            <a href=@{IssueR issueId}>#{toPathPiece issueId}
            $if issueIsClosed
                <span .badge.bg-danger>closed
        <td>
            $forall (asset, amount) <- Map.assocs amounts
                #{show amount} #{showKnownAsset asset} <br>
    |]

-- elide :: Int -> Int -> Text -> Text
-- elide a b t = Text.take a t <> "…" <> Text.takeEnd b t

-- stellarExpertTx :: TxId -> Widget
-- stellarExpertTx txId = [whamlet|<a href=#{href}>#{shortTxId}|] where
--     href = "https://stellar.expert/explorer/public/tx/" <> toPathPiece txId
--     shortTxId = elide 4 4 $ toUrlPiece txId
