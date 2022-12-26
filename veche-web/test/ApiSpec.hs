{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (spec) where

-- prelude
import TestImport

-- global
import Data.ByteString (breakSubstring)
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
    describe "verification key" do

        it "taken from /start command" do
            get' $ AuthR LoginR
            statusIs 200

            a <-
                htmlQuery ".mymtlwalletbot-authn-start"
                <&> toStrict . headEx
            let verifier = between "?start=veche_" "\"" a
            let signature =
                    signBlob kp $ encodeUtf8 account <> verifier

            get'
                (   mmwbR []
                ,   [ ("account", account)
                    , ("signature", decodeUtf8Throw $ Base64.encode signature)
                    ]
                )
            statusIs 303 -- successful authentication

        it "is created on request" do
            post' (mmwbR ["verifier"], [("account", account)]) []
            statusIs 200
            verifier <- getResponseBody

            let signature =
                    signBlob kp $ encodeUtf8 account <> toStrict verifier

            testClearCookies -- important! clear session
            get'
                (   mmwbR []
                ,   [ ("account", account)
                    , ("signature", decodeUtf8Throw $ Base64.encode signature)
                    , ("verifier", decodeUtf8Throw $ toStrict verifier)
                    ]
                )
            statusIs 303 -- successful authentication

between :: ByteString -> ByteString -> ByteString -> ByteString
between start end =
    fst . breakSubstring end . drop (length start) . snd . breakSubstring start
