{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Workers.StellarUpdate (stellarDataUpdater) where

import Import hiding (cached)

-- global
import Control.Concurrent (threadDelay)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (BaseUrl, ClientEnv, ClientM, mkClientEnv, runClientM)
import System.Random (randomRIO)

-- project
import Stellar.Horizon.Client (getAccount, getAllAccounts)
import Stellar.Horizon.Types (Account (Account), Asset (Asset), Signer (Signer),
                              SignerType (Ed25519PublicKey))
import Stellar.Horizon.Types qualified

-- component
import Genesis (mtlAsset, mtlFund)
import Model.Vote qualified as Vote

stellarDataUpdater :: BaseUrl -> ConnectionPool -> IO ()
stellarDataUpdater baseUrl connPool = do
    manager <- newTlsManager
    let clientEnv = mkClientEnv manager baseUrl
    forever do
        do  putStrLn "stellarDataUpdater: Updating MTL signers"
            n <- updateSignersCache clientEnv connPool mtlFund
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL signers"
        do  putStrLn "stellarDataUpdater: Updating MTL holders"
            n <- updateHoldersCache clientEnv connPool mtlAsset
            putStrLn $
                "stellarDataUpdater: Updated " <> tshow n <> " MTL signers"
        randomDelay
  where
    randomDelay = do
        delayMinutes <- randomRIO (1, 10)
        threadDelay $ delayMinutes * 60 * 1_000_000

updateSignersCache ::
    ClientEnv ->
    ConnectionPool ->
    Text ->
    -- | Actual number of signers
    IO Int
updateSignersCache clientEnv connPool target = do
    Account{signers} <- runClientM' clientEnv $ getAccount target
    let actual =
            Map.fromList
                [ (key, weight)
                | Signer{key, weight, type_ = Ed25519PublicKey} <- signers
                , weight > 0
                ]
    (`runSqlPool` connPool) do
        cached' <- selectList [StellarSignerTarget ==. target] []
        let cached =
                Map.fromList
                    [ (stellarSignerKey, stellarSignerWeight)
                    | Entity{entityVal} <- cached'
                    , let
                        StellarSigner
                            {stellarSignerKey, stellarSignerWeight} =
                                entityVal
                    ]
        handleDeleted $ Map.keys $ cached \\ actual
        handleAdded $ actual \\ cached
        handleModified
            [ (key, weightActual)
            | (key, (weightCached, weightActual)) <-
                Map.assocs $ Map.intersectionWith (,) cached actual
            , weightCached /= weightActual
            ]
        unless (cached == actual)
            updateAllIssueApprovals
    pure $ length actual
  where

    handleDeleted = traverse_ $ deleteBy . UniqueMember target

    handleAdded added =
        insertMany_
            [ StellarSigner
                { stellarSignerTarget = target
                , stellarSignerKey
                , stellarSignerWeight
                }
            | (stellarSignerKey, stellarSignerWeight) <- Map.assocs added
            ]

    handleModified =
        traverse_ \(key, weight) ->
            updateWhere
                [StellarSignerTarget ==. target, StellarSignerKey ==. key]
                [StellarSignerWeight =. weight]

    updateAllIssueApprovals = do
        issues <- selectList [] []
        for_ issues \(Entity issueId issue) ->
            Vote.updateIssueApproval issueId $ Just issue

updateHoldersCache ::
    ClientEnv ->
    ConnectionPool ->
    Asset ->
    -- | Actual number of holders
    IO Int
updateHoldersCache clientEnv connPool asset@Asset{code, issuer} = do
    holders <- runClientM' clientEnv $ getAllAccounts asset
    let actual = Set.fromList [account_id | Account{account_id} <- holders]
    (`runSqlPool` connPool) do
        cached' <-
            selectList
                [ StellarHolderAssetCode    ==. code
                , StellarHolderAssetIssuer  ==. issuer
                ]
                []
        let cached =
                Set.fromList
                    [ stellarHolderKey
                    | Entity _ StellarHolder{stellarHolderKey} <- cached'
                    ]
        handleDeleted $ cached \\ actual
        handleAdded $ actual \\ cached
    pure $ length actual

  where

    handleDeleted = traverse_ $ deleteBy . UniqueHolder code issuer

    handleAdded added =
        insertMany_
            [ StellarHolder
                { stellarHolderAssetCode    = code
                , stellarHolderAssetIssuer  = issuer
                , stellarHolderKey
                }
            | stellarHolderKey <- toList added
            ]

runClientM' :: ClientEnv -> ClientM a -> IO a
runClientM' clientEnv action =
    runClientM action clientEnv >>= either throwIO pure
