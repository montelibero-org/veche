{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString.Base64 qualified as Base64
import Data.Function
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.IO qualified as Text
import Network.HTTP.Client.TLS
import Network.ONCRPC.XDR
import Network.Stellar.Builder
import Network.Stellar.Keypair
import Network.Stellar.Network
import Network.Stellar.TransactionXdr
import Servant.Client
import Stellar.Horizon.Client (Account (Account), Address (Address), getAccount,
                               publicServerBase, submitTransaction)
import Stellar.Horizon.Client qualified
import Text.Read

defaultOptions :: SetOptionsOp
defaultOptions =
    SetOptionsOp
        { setOptionsOp'inflationDest    = Nothing
        , setOptionsOp'clearFlags       = Nothing
        , setOptionsOp'setFlags         = Nothing
        , setOptionsOp'masterWeight     = Nothing
        , setOptionsOp'lowThreshold     = Nothing
        , setOptionsOp'medThreshold     = Nothing
        , setOptionsOp'highThreshold    = Nothing
        , setOptionsOp'homeDomain       = Nothing
        , setOptionsOp'signer           = Nothing
        }

main :: IO ()
main = do
    manager <- newTlsManager
    let clientEnv = mkClientEnv manager publicServerBase
    Right Account{sequence = sequenceText} <-
        runClientM (getAccount $ Address addressText) clientEnv
    sequenceInt <- either fail pure $ readEither $ Text.unpack sequenceText

    let tx =
            transactionBuilder (decodePublicKey' addressText) (sequenceInt + 1)
            & (`addOperation` op)
            & build

    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    Right envelope <-
        pure $ sign publicNetwork (toEnvelope tx) [fromPrivateKey' secret]
    let envelopeXdrBase64 = decodeUtf8 $ Base64.encode $ xdrSerialize envelope
    Text.putStrLn $ "envelope = " <> envelopeXdrBase64

    runClientM (submitTransaction envelopeXdrBase64) clientEnv >>= print
  where
    addressText = "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"
    op = Operation{..}
    operation'sourceAccount = Nothing
    operation'body =
        OperationBody'SET_OPTIONS $
        defaultOptions
            { setOptionsOp'homeDomain =
                Just $ lengthArray' "veche.montelibero.org"
            }
