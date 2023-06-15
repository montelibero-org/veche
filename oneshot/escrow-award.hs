{-# OPTIONS -Wno-missing-signatures #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Named

import Stellar.Simple

main = do
    client <- getPublicClient ! defaults
    t <-
        transactionBuilder escrow
        & tx_feePerOp_guess
        & tx_memoText "E12"
        & op_payment eurmtl 0.98e7 ! #destination performer ! defaults
        & op_payment eurmtl 0.02e7 ! #destination veche     ! defaults
        & build client
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    Text.putStrLn $ signWithSecret secret t & xdrSerializeBase64T

escrow = Address "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"

eurmtl =
    mkAsset "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"

performer = Address "GACNFOLV3ATA6N6AHMO3IBZCYMHUMFCT6O452DW3RTU254TZJ5CP3V3Q"

veche = Address "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
