{-# OPTIONS -Wno-missing-signatures #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time
import Named

import Stellar.Simple hiding (issuer)

main = do
    client <- getPublicClient ! defaults
    retryOnTimeout do

        t <-
            transactionBuilder (Address issuer)
            & tx_seqNum 187623215374270465
            & tx_feePerOp_guess
            & op_payment user gem 200e7
            & build client

        secret <- Text.strip <$> Text.readFile "/tmp/secret"
        let envelope = signWithSecret secret t
        Text.putStrLn $ "envelope: " <> xdrSerializeBase64T envelope

        putStrLn "submitting..."
        print =<< getCurrentTime
        submit envelope client >>= print

issuer = "GAZI7H2IZ2YM6DCOGX4UYVDCDCHV76M4VEDKKZUCGZ45GCBK3Q5E24S3"

gem = mkAsset "GEM" issuer

user = Address "GD7AXEICJMMT7POF5SJHGNVFRJIZWOXMGP67LPCWIUT4DT6WRUHH6DZF"
