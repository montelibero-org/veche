{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Base64 qualified as Base64
import Data.Function
import Data.Text
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.IO qualified as Text
import Network.HTTP.Client.TLS
import Network.ONCRPC.XDR
import Network.Stellar.Builder
import Network.Stellar.Keypair
import Network.Stellar.Network
import Network.Stellar.Operation
import Network.Stellar.TransactionXdr
import Servant.Client (mkClientEnv, runClientM)
import Stellar.Horizon.Client (Account (Account), Address (Address), getAccount,
                               publicServerBase)
import Stellar.Horizon.Client qualified
import Text.Read

main :: IO ()
main = do
    manager <- newTlsManager
    let clientEnv = mkClientEnv manager publicServerBase
    Right Account{sequence = sequenceText} <-
        runClientM (getAccount $ Address escrowAddressText) clientEnv
    sequenceInt <- either fail pure $ readEither $ Text.unpack sequenceText

    let tx =
            (transactionBuilder
                (decodePublicKey' escrowAddressText) (sequenceInt + 1)
            )
                {tbMemo = Just $ Memo'MEMO_TEXT "E17"}
            `addOperation` makePaymentOperation addr1 eurmtl 19.6e7
            `addOperation` makePaymentOperation addr2 eurmtl  0.4e7
            & build

    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    Right envelope <-
        pure $ sign publicNetwork (toEnvelope tx) [fromPrivateKey' secret]
    let envelopeXdrBase64 = decodeUtf8 $ Base64.encode $ xdrSerialize envelope
    Text.putStrLn $ "envelope = " <> envelopeXdrBase64

--     runClientM (submitTransaction envelopeXdrBase64) clientEnv >>= print

  where
    escrowAddressText =
        "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"

    addr1 =
        muxedAccount "GDLTH4KKMA4R2JGKA7XKI5DLHJBUT42D5RHVK6SS6YHZZLHVLCWJAYXI"

    addr2 =
        muxedAccount "GCPT3X4FJBMUBR5AIB7SEUQX7HJ4XX3K4TNI2J7WIHMHMFGDMRRJJVWL"

-- buildMuxedAccount :: C.PublicKey -> MuxedAccount
muxedAccount :: Text -> MuxedAccount
muxedAccount = MuxedAccount'KEY_TYPE_ED25519 . lengthArray' . decodePublic'

eurmtl :: Asset
eurmtl =
    Asset'ASSET_TYPE_CREDIT_ALPHANUM12
    AlphaNum12
    { alphaNum12'assetCode = padLengthArray "EURMTL" 0
    , alphaNum12'issuer =
        buildAccount $
        decodePublicKey'
            "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
    }
