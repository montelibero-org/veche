{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- @
-- do  manager <- newTlsManager
--     eResult <- runClientM req $ mkClientEnv manager publicServerBase
--     either throwIO pure eResult
-- @
module Stellar.Horizon.Client
    (
    -- * Endpoints
      publicServerBase
    , testServerBase
    -- * Methods
    , getAccount
    , getAccounts
    , getAccountTransactions
    , getAccountsList
    , getAccountTransactionsList
    ) where

import Prelude hiding (last)

-- global
import Data.List.NonEmpty (last, nonEmpty)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl, ClientM, client, parseBaseUrl)
import System.IO.Unsafe (unsafePerformIO)

-- component
import Stellar.Horizon.API (api)
import Stellar.Horizon.Types (Account, Address, Asset, Record (Record),
                              Records (Records), Transaction)
import Stellar.Horizon.Types qualified

publicServerBase :: BaseUrl
publicServerBase = unsafePerformIO $ parseBaseUrl "https://horizon.stellar.org/"
{-# NOINLINE publicServerBase #-}

testServerBase :: BaseUrl
testServerBase =
    unsafePerformIO $ parseBaseUrl "https://horizon-testnet.stellar.org/"
{-# NOINLINE testServerBase #-}

getAccounts ::
    Maybe Asset -> Maybe Text -> Maybe Natural -> ClientM (Records Account)
getAccount :: Address -> ClientM Account
getAccountTransactions ::
    Address -> Maybe Text -> Maybe Natural -> ClientM (Records Transaction)
getAccounts :<|> getAccount :<|> getAccountTransactions = client api

getAccountsList :: Asset -> ClientM [Account]
getAccountsList = recordsToList . getAccounts . Just

getAccountTransactionsList :: Address -> ClientM [Transaction]
getAccountTransactionsList = recordsToList . getAccountTransactions

recordsToList ::
    (Maybe Text -> Maybe Natural -> ClientM (Records a)) -> ClientM [a]
recordsToList endpoint = go Nothing where
    limit = 200
    go cursor = do
        Records records <- endpoint cursor (Just limit)
        let values = map (\Record{value} -> value) records
        case nonEmpty records of
            Just neRecords | length records == fromIntegral limit ->
                let Record{paging_token} = last neRecords
                in (values <>) <$> go (Just paging_token)
            _ -> pure values
