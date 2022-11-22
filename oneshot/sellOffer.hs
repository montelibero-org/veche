{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time
import Named
import Network.HTTP.Client
import Stellar.Simple

main = do
    client <-
        getPublicClient ! #responseTimeout (responseTimeoutMicro 60_000_000)
    tx <-
        transactionBuilder distibutor
        & tx_feePerOp 1_000
        & op_manageSellOffer (#selling vecheToken) (#buying eurmtl) 146e7 1
        & build client
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret tx
    putStrLn "envelope:"
    Text.putStrLn $ xdrSerializeBase64T envelope
    retryOnTimeout do
        print =<< getCurrentTime
        putStrLn "submitting..."
        submit envelope client >>= print

distibutor = Address "GD5TGAX3IM2EFA4PBTDNHAM2U5UPAV6DYVR6TA2KCONFFSUOID3ZB6HA"

vecheToken =
    mkAsset "VECHE" "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"

eurmtl =
    mkAsset "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
