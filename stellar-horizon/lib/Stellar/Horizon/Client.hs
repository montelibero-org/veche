{-# LANGUAGE DisambiguateRecordFields #-}
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
    , getAllAccounts
    ) where

import Prelude hiding (last)

import Data.Text (Text)
import Numeric.Natural (Natural)
import Servant.Client (BaseUrl, ClientM, client, parseBaseUrl)
import System.IO.Unsafe (unsafePerformIO)

import Data.List.NonEmpty (last, nonEmpty)
import Servant.API ((:<|>) ((:<|>)))
import Stellar.Horizon.API (api)
import Stellar.Horizon.Types (Account (Account, paging_token), Asset,
                              Records (Records))
import Stellar.Horizon.Types qualified

publicServerBase :: BaseUrl
publicServerBase = unsafePerformIO $ parseBaseUrl "https://horizon.stellar.org/"
{-# NOINLINE publicServerBase #-}

testServerBase :: BaseUrl
testServerBase =
    unsafePerformIO $ parseBaseUrl "https://horizon-testnet.stellar.org/"
{-# NOINLINE testServerBase #-}

getAccount :: Text -> ClientM Account
getAccounts ::
    Maybe Asset -> Maybe Text -> Maybe Natural -> ClientM (Records Account)
getAccount :<|> getAccounts = client api

getAllAccounts :: Asset -> ClientM [Account]
getAllAccounts asset = go Nothing where
    limit = 200
    go cursor = do
        Records{_embedded_records} <-
            getAccounts (Just asset) cursor (Just limit)
        case nonEmpty _embedded_records of
            Just neRecords | length _embedded_records == fromIntegral limit ->
                let Account{paging_token} = last neRecords
                in (_embedded_records <>) <$> go (Just paging_token)
            _ -> pure _embedded_records
