{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Stellar.Simple

main :: IO ()
main = do
    t <-
        transactionBuilder escrow
        & setMemoText "E17"
        & addPayment dest1 eurmtl 19.6e7
        & addPayment dest2 eurmtl  0.4e7
        & build
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret t & transactionEnvelopeXdrBase64T
    Text.putStrLn $ "envelope = " <> envelope
--     runClientM (submitTransaction envelopeXdrBase64) clientEnv >>= print
  where
    escrow =
        Address "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"
    eurmtl =
        mkAsset
            "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
    dest1 = Address "GDLTH4KKMA4R2JGKA7XKI5DLHJBUT42D5RHVK6SS6YHZZLHVLCWJAYXI"
    dest2 = Address "GCPT3X4FJBMUBR5AIB7SEUQX7HJ4XX3K4TNI2J7WIHMHMFGDMRRJJVWL"
