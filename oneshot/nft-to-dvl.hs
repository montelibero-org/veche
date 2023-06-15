{-# OPTIONS -Wno-missing-signatures #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Named

import Stellar.Simple hiding (issuer)

main = do
    client <- getPublicClient ! defaults

    envelope <-
        transactionBuilder (Address smithy)
        & tx_feePerOp_guess
        &.. [ op_payment (mkToken a) 1 ! #destination dvl ! defaults
            | a <- [1..8]
            ]
        & build client

    Text.putStrLn $ "envelope: " <> xdrSerializeBase64T envelope

smithy = "GDPHAKGLJ3B56BK4CZ2VMTYEDI6VZ2CTHUHSFAFSPGSTJHZEI3ATOKEN"

mkToken :: Int -> Asset
mkToken a =
    mkAsset
        ("BP1369p12a" <> Text.pack (show a))
        "GDDTITOAZSV6OFHQJ5H2BALN7SNF4RGKZLSEIUA4RTJK44VCWTXEPNFT"

dvl = Address "GC6IQK47LJJNQKZUCSOUQNSULPJPYMZ47TGUPMLBC4JESV43XBR7GZMP"

x &.. fs = foldl (&) x fs
infixl 1 &..
