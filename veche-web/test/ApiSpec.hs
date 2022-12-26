{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (spec) where

-- prelude
import TestImport

-- global
import Data.ByteString.Base64 qualified as Base64
import Network.Stellar.Keypair (KeyPair, encodePublicKey, fromPrivateKey')
import Network.Stellar.Keypair qualified
import Network.Stellar.Signature (signBlob)

mmwbR :: [Text] -> Route App
mmwbR = AuthR . PluginR "mymtlwalletbot"

kp :: KeyPair
kp = fromPrivateKey' "SDHDJTXDM6IILXA3SPT357QJ2MJCH6WWWLZDVZI2HHSIR3V2E4AOMG4B"

account :: Text
account = encodePublicKey kp.kpPublicKey

spec :: Spec
spec =
    withApp $
        describe "verification key" $
            it "is created on request" do
                post' (mmwbR ["verifier"], [("account", account)]) []
                statusIs 200
                verifier <- getResponseBody

                let signature =
                        signBlob kp $ encodeUtf8 account <> toStrict verifier

                get'
                    (   mmwbR []
                    ,   [ ("account", account)
                        , ("signature",
                            decodeUtf8Throw $ Base64.encode signature)
                        ]
                    )
                statusIs 303
