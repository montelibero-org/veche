{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy (..))
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.CLI (parseClient)
import Servant.Client (ClientError (FailureResponse), ClientM, mkClientEnv,
                       runClientM)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Pretty.Simple (pHPrint, pPrint)

import Stellar.Horizon.API (api)
import Stellar.Horizon.Client (publicServerBase)

main :: IO ()
main = do
    client <- parseClient api (Proxy @ClientM) mempty
    manager <- newTlsManager
    res <- runClientM client $ mkClientEnv manager publicServerBase
    case res of
        Left (FailureResponse _ response) -> do
            putStrLn "FailureResponse"
            pHPrint stderr response
            exitFailure
        Left err -> do
            pHPrint stderr err
            exitFailure
        Right result -> pPrint result
