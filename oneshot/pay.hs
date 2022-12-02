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
            & tx_seqNum 186___0
            & tx_feePerOp_guess
            & op_payment user token 200e7
            & build client

        secret <- Text.strip <$> Text.readFile "/tmp/secret"
        let envelope = signWithSecret secret t
        Text.putStrLn $ "envelope: " <> xdrSerializeBase64T envelope

        putStrLn "submitting..."
        print =<< getCurrentTime
        submit envelope client >>= print

issuer = "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"

token = mkAsset "VECHE" issuer

user = Address "G..."
