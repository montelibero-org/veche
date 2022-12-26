{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

import Data.Text qualified as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Stellar.Keypair (decodePublicKey)
import Options.Applicative (maybeReader)
import Options.Applicative.Types (readerAsk)
import Options.Generic (Generic, ParseField, ParseFields, ParseRecord,
                        getRecord)
import Options.Generic qualified
import Servant.Client (ClientError (FailureResponse), ClientM, mkClientEnv,
                       runClientM)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Pretty.Simple (pHPrint, pPrint)

import Stellar.Horizon.Client (Address (Address), Asset, assetFromText,
                               getAccount, getAccountsList, getFeeStats,
                               publicServerBase)

instance ParseField Address where
    readField = maybeReader $ (\t -> Address t <$ decodePublicKey t) . Text.pack

instance ParseFields Address
instance ParseRecord Address

instance ParseField Asset where
    readField = assetFromText . Text.pack <$> readerAsk

instance ParseFields Asset
instance ParseRecord Asset

data Input
    = Account Address
    | Accounts{asset :: Asset}
    | Fee_stats
    deriving (Generic)

instance ParseRecord Input

cli :: Input -> ClientM ()
cli = \case
    Account account -> getAccount account    >>= pPrint
    Accounts{asset} -> getAccountsList asset >>= pPrint
    Fee_stats       -> getFeeStats           >>= pPrint

main :: IO ()
main = do
    manager <- newTlsManager
    input <- getRecord "Stellar Horizon client"
    res <- runClientM (cli input) $ mkClientEnv manager publicServerBase
    case res of
        Left (FailureResponse _ response) -> do
            putStrLn "FailureResponse"
            pHPrint stderr response
            exitFailure
        Left err -> do
            pHPrint stderr err
            exitFailure
        Right () -> pure ()
