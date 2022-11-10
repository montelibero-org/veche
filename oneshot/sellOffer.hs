{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Named ((!))
import Stellar.Simple

main :: IO ()
main = do
    t <-
        transactionBuilder distibutor
        & (op_manageSellOffer ! #selling vecheToken ! #buying eurmtl) 146 1
        & build
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    Text.putStrLn $ t & signWithSecret secret & xdrSerializeBase64T
  where
    distibutor =
        Address "GD5TGAX3IM2EFA4PBTDNHAM2U5UPAV6DYVR6TA2KCONFFSUOID3ZB6HA"
    vecheToken =
        mkAsset
            "VECHE" "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
    eurmtl =
        mkAsset
            "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
