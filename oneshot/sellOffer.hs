{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time
import Data.Typeable
import Named
import Network.HTTP.Client
import Network.HTTP.Types (gatewayTimeout504)
import Servant.Client (ClientError (ConnectionError, FailureResponse),
                       ResponseF (Response))
import Servant.Client qualified
import Stellar.Simple
import WithCallStack

main = do
    client <-
        getPublicClient ! #responseTimeout (responseTimeoutMicro 60_000_000)
    tx <-
        client
        & transactionBuilder distibutor
        & tx_feePerOp 1_000
        & op_manageSellOffer (#selling vecheToken) (#buying eurmtl) 146e7 1
        & build
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret tx
    putStrLn "envelope:"
    Text.putStrLn $ xdrSerializeBase64T envelope
    retryOnTimeout do
        putStrLn "submitting..."
        submit envelope client >>= print

distibutor = Address "GD5TGAX3IM2EFA4PBTDNHAM2U5UPAV6DYVR6TA2KCONFFSUOID3ZB6HA"

vecheToken =
    mkAsset "VECHE" "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"

eurmtl =
    mkAsset "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"

retryOnTimeout :: IO a -> IO a
retryOnTimeout action = catchJust guardTimeout action next where

    guardTimeout = \case
        e1@WithCallStack{parent = SomeException e2}
            | Just (ConnectionError (SomeException e3)) <- cast e2
            , Just (HttpExceptionRequest _req ResponseTimeout) <- cast e3
            ->
                Just e1
        e1@WithCallStack{parent = SomeException e2}
            | Just (FailureResponse _req Response{responseStatusCode}) <-
                cast e2
            , responseStatusCode == gatewayTimeout504
            ->
                Just e1
        _ -> Nothing

    next e = do
        print =<< getCurrentTime
        putStrLn $ displayException e
        putStrLn "retrying..."
        retryOnTimeout action
