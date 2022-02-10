{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

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

stellarDataUpdater :: BaseUrl -> ConnectionPool -> IO ()
stellarDataUpdater baseUrl connPool =
    forever do
        putStrLn "stellarDataUpdater: Updating MTL signers"
        n <- updateMtlSignersCache mtlFund
        putStrLn $ "stellarDataUpdater: Updated " <> tshow n <> " items"
        randomDelay
  where

    updateMtlSignersCache target = do
        Account{signers} <- getAccount' target
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
            let deleted = cached \\ actual
            let added   = actual \\ cached
            let modified =
                    Map.fromList
                        [ (key, weightActual)
                        | (key, (weightCached, weightActual)) <-
                            Map.assocs $ Map.intersectionWith (,) cached actual
                        , weightCached /= weightActual
                        ]
            for_ (Map.keys deleted) $ deleteBy . UniqueSigner target
            insertMany_
                [ StellarSigner
                    { stellarSignerTarget = target
                    , stellarSignerKey
                    , stellarSignerWeight
                    }
                | (stellarSignerKey, stellarSignerWeight) <- Map.assocs added
                ]
            for_ (Map.assocs modified) \(key, weight) ->
                updateWhere
                    [StellarSignerTarget ==. target, StellarSignerKey ==. key]
                    [StellarSignerWeight =. weight]

    getAccount' address = do
        manager <- newTlsManager
        eResult <- runClientM (getAccount address) $ mkClientEnv manager baseUrl
        case eResult of
            Left err -> throwIO err
            Right result -> pure result

    randomDelay = do
        delayMinutes <- randomRIO (1, 60)
        threadDelay $ delayMinutes * 60 * 1_000_000

    db action = runSqlPool action connPool
