{-# OPTIONS -Wno-missing-signatures #-}

import Data.Function
import Data.Map qualified as Map
import Data.Text.IO qualified as Text
import Data.Time
import Named

import Stellar.Simple hiding (issuer)

main = do
    client <- getPublicClient ! defaults
    retryOnTimeout do

        t <-
            transactionBuilder (Address issuerPub)
            -- & tx_feePerOp_guess
            -- & op_setHomeDomain "assets.montelibero.org"
            -- & op_payment token 20_000_e7
            --     ! #destination (Address distributorPub)
            --     ! #source (Address issuerPub)
            -- & op_manageSellOffer
            --     (#selling token)
            --     (#unit eurmtl)
            --     (#amount 10_000_e7)
            --     (#price 1)
            --     ! defaults
            & op_setSigners
                (Map.fromList [(Address addr, 1) | addr <- adminPubKeys])
            & op_setThresholds (#low 1) (#med 1) (#high 1)
            & op_setMasterWeight 0
            & build client

        -- secret <- Text.strip <$> Text.readFile "/tmp/secret"
        let envelope = t & signWithSecret issuerSec -- & signWithSecret operatorSec
        Text.putStrLn $ "envelope:\n" <> xdrSerializeBase64T envelope

        putStrLn "submitting..."
        print =<< getCurrentTime
        submit envelope client >>= print

issuerPub = "GCGWAPG6PKBMHEEAHRLTWHFCAGZTQZDOXDMWBUBCXHLQBSBNWFRYFEST"
issuerSec = undefined

-- token = mkAsset "MTLFEST" issuerPub

-- distributorPub = "GBJ4BPR6WESHII6TO4ZUQBB6NJD3NBTK5LISVNKXMPOMMYSLR5DOXMFD"

adminPubKeys =
    [ "GCPJUXPETZEIJNEAAEO2LGZIA6IQNNWGOHPNP4ZQECDS6IKKIGJO5LFV"
    , "GCPT3X4FJBMUBR5AIB7SEUQX7HJ4XX3K4TNI2J7WIHMHMFGDMRRJJVWL"
    ]

-- eurmtl =
--     mkAsset "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
