import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Named

import Stellar.Simple

main :: IO ()
main = do
    client <- getPublicClient ! defaults
    t <-
        client
        & transactionBuilder escrow
        & tx_memoText "E12"
        & op_payment dest1 eurmtl 0.98e7
        & op_payment dest2 eurmtl 0.02e7
        & build
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    Text.putStrLn $ signWithSecret secret t & xdrSerializeBase64T
  where
    escrow =
        Address "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"
    eurmtl =
        mkAsset
            "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
    dest1 = Address "GACNFOLV3ATA6N6AHMO3IBZCYMHUMFCT6O452DW3RTU254TZJ5CP3V3Q"
    dest2 = Address "GCPT3X4FJBMUBR5AIB7SEUQX7HJ4XX3K4TNI2J7WIHMHMFGDMRRJJVWL"
