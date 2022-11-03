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
module Stellar.Horizon.Client (
    -- * Types
    Account (..),
    Address (..),
    Asset (..),
    assetFromText,
    assetToText,
    Memo (..),
    Operation (..),
    Signer (..),
    Transaction (..),
    TransactionOnChain (..),
    transactionFromDto,
    transactionFromEnvelopeXdr,
    TxId (..),
    -- * Endpoints
    publicServerBase,
    testServerBase,
    -- * Methods
    getAccount,
    getAccounts,
    getAccountTransactionsDto,
    getAccountsList,
    getAccountTransactionsDtoList,
    getAccountTransactionsList,
    submitTransaction,
) where

import Prelude hiding (last)

-- global
import Data.List.NonEmpty (last, nonEmpty)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl, ClientM, client, parseBaseUrl)
import System.IO.Unsafe (unsafePerformIO)

-- component
import Stellar.Horizon.API (api)
import Stellar.Horizon.DTO (Account (..), Address (..), Record (Record),
                            Records (Records), Signer (..), TxId (..))
import Stellar.Horizon.DTO qualified as DTO
import Stellar.Simple (Asset (..), Memo (..), Operation (..), Transaction (..),
                       TransactionOnChain (..), assetFromText, assetToText,
                       transactionFromDto, transactionFromEnvelopeXdr)

-- | Public network Horizon server https://horizon.stellar.org/
publicServerBase :: BaseUrl
publicServerBase = unsafePerformIO $ parseBaseUrl "https://horizon.stellar.org/"
{-# NOINLINE publicServerBase #-}

-- | Test network Horizon server https://horizon-testnet.stellar.org/
testServerBase :: BaseUrl
testServerBase =
    unsafePerformIO $ parseBaseUrl "https://horizon-testnet.stellar.org/"
{-# NOINLINE testServerBase #-}

getAccounts ::
    Maybe Asset -> Maybe Text -> Maybe Natural -> ClientM (Records Account)
getAccount :: Address -> ClientM Account
getAccountTransactionsDto ::
    Address -> Maybe Text -> Maybe Natural -> ClientM (Records DTO.Transaction)
submitTransaction :: Text -> ClientM DTO.Transaction
(               getAccounts
        :<|>    getAccount
        :<|>    getAccountTransactionsDto
        :<|>    submitTransaction) =
    client api

getAccountsList :: Asset -> ClientM [Account]
getAccountsList = recordsToList . getAccounts . Just

getAccountTransactionsDtoList :: Address -> ClientM [DTO.Transaction]
getAccountTransactionsDtoList = recordsToList . getAccountTransactionsDto

getAccountTransactionsList ::
    HasCallStack => Address -> ClientM [TransactionOnChain]
getAccountTransactionsList =
    fmap (map transactionFromDto) . getAccountTransactionsDtoList

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
