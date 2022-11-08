{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Stellar.Simple

main :: IO ()
main = do
    t <-
        transactionBuilder escrow
        & addSetHomeDomain "veche.montelibero.org"
        & build
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = t & signWithSecret secret & transactionEnvelopeXdrBase64T
    Text.putStrLn envelope
  where
    escrow = Address "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"
