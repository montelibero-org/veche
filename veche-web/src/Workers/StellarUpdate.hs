{-# LANGUAGE BlockArguments #-}
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
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (BaseUrl, mkClientEnv, runClientM)
import System.Random (randomRIO)

-- project
import Stellar.Horizon.Client (getAccount)
import Stellar.Horizon.Types (Account (..), Signer (..),
                              SignerType (Ed25519PublicKey))

-- component
import Genesis (mtlFund)
import Model.Vote qualified as Vote

stellarDataUpdater :: BaseUrl -> ConnectionPool -> IO ()
stellarDataUpdater baseUrl connPool =
    forever do
        putStrLn "stellarDataUpdater: Updating MTL signers"
        n <- updateMtlSignersCache baseUrl connPool mtlFund
        putStrLn $ "stellarDataUpdater: Updated " <> tshow n <> " items"
        randomDelay
  where
    randomDelay = do
        delayMinutes <- randomRIO (1, 10)
        threadDelay $ delayMinutes * 60 * 1_000_000

updateMtlSignersCache ::
    BaseUrl ->
    ConnectionPool ->
    Text ->
    -- | Actual number of signers
    IO Int
updateMtlSignersCache baseUrl connPool target = do
    Account{signers} <- getAccount' baseUrl target
    let actual =
            Map.fromList
                [ (key, weight)
                | Signer{key, weight, type_ = Ed25519PublicKey} <- signers
                , weight > 0
                ]
    db do
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

    db action = runSqlPool action connPool

    updateAllIssueApprovals = do
        issues <- selectList [] []
        for_ issues \(Entity issueId issue) ->
            Vote.updateIssueApproval issueId $ Just issue

getAccount' :: BaseUrl -> Text -> IO Account
getAccount' baseUrl address = do
    manager <- newTlsManager
    eResult <- runClientM (getAccount address) $ mkClientEnv manager baseUrl
    case eResult of
        Left err -> throwIO err
        Right result -> pure result
