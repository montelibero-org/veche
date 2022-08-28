{-# LANGUAGE DisambiguateRecordFields #-}
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
        Records records <- getAccounts (Just asset) cursor (Just limit)
        case nonEmpty records of
            Just neRecords | length records == fromIntegral limit ->
                let Account{paging_token} = last neRecords
                in (records <>) <$> go (Just paging_token)
            _ -> pure records
