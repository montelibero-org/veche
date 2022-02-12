{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.StellarSpec (spec) where

import TestImport

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Except (throwError)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server (Server, err404, serve)
import Servant.Server qualified as Servant

-- project
import Stellar.Horizon.API (API, api)
import Stellar.Horizon.Types (Account (..), Signer (..),
                              SignerType (Ed25519PublicKey))

spec :: Spec
spec =
    around_ withMockHorizon $
    withApp do
        describe "Auth.Stellar" do

            describe "initial public key form" do

                it "shows public key form" do
                    get $ AuthR LoginR
                    statusIs 200
                    htmlCount "input[name=stellar_address]" 1

            describe "challenge/response form" do

                it "shows challenge for address" do
                    get (AuthR LoginR, [("stellar_address", testGoodPublicKey)])
                    statusIs 200
                    htmlCount ".stellar_challenge" 1

                it "shows error for bad address" do
                    get (AuthR LoginR, [("stellar_address", "")])
                    statusIs 400

                it "shows error for invalid address" do
                    get (AuthR LoginR, [("stellar_address", "foo")])
                    statusIs 400

            describe "authentication with tx-response" do

                it "authenticates when correct tx-response (mainnet)" $
                    testAuthenticationOk
                        testGoodPublicKey
                        testGoodTxSignedForMainnet

                it "authenticates when correct tx-response (testnet)" $
                    testAuthenticationOk
                        testGoodPublicKey
                        testGoodTxSignedForTestnet

                it "doesn't authenticate when incorrect tx-response" do
                    request do
                        setMethod "POST"
                        setUrl $ AuthR $ PluginR "stellar" []
                        addPostParam "response" testGoodTxUnsinged
                    statusIs 400

testAuthenticationOk :: Text -> Text -> YesodExample App ()
testAuthenticationOk address tx = do
    get (AuthR LoginR, [("stellar_address", address)])
    statusIs 200

    request do
        setMethod "POST"
        setUrl $ AuthR $ PluginR "stellar" []
        addPostParam "response" tx
        addToken_ "#auth_stellar_response_form"
    statusIs 303 -- okay redirect

    get UserR
    statusIs 200 -- authenticated

    users <- runDB $ fmap entityVal <$> selectList [] []
    assertEq
        "users after auth"
        users
        [User{userName = Nothing, userStellarAddress = testGoodPublicKey}]

testGoodPublicKey :: Text
testGoodPublicKey = "GDLNZNS3HM3I3OAQKYV5WBACXCUU7JWEQLGGCAEV7E4LA4BY2XFOLEWX"

-- testGoodSecretKey =
--     "SA3KFUDKK5YDG2X2OKLW26JSTPGPUL6DOODIYZ4A2C4ZI3Y5RG2YJQVW"

testGoodTxUnsinged :: Text
testGoodTxUnsinged =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAA"

testGoodTxSignedForMainnet :: Text
testGoodTxSignedForMainnet =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAB\
    \ONXK5QAAAEAy13Yo6c30JzWbUp24b43kCtUitlwbtijOhmU0H6cZlFm8xCa2Y9Eu\
    \WvCG0Rima5rIXHTJnPKHJdI6boKpODwM"

testGoodTxSignedForTestnet :: Text
testGoodTxSignedForTestnet =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAB\
    \ONXK5QAAAEDxIVAlHINvvv9/BcFP6kvXd0Tw/F1w8jEFxVf8UISvDxw6SOmt3Z6B\
    \Nm/CMBwyBtYXYooKhoKW6ky8A0hStCMH"

withMockHorizon :: IO () -> IO ()
withMockHorizon =
    bracket (forkIO $ Warp.run 9999 horizonTestApp) killThread . const

horizonTestApp :: Wai.Application
horizonTestApp = serve api horizonTestServer
  where

    horizonTestServer :: Server API
    horizonTestServer = getAccount

    getAccount :: Text -> Servant.Handler Account
    getAccount address
        | address == testGoodPublicKey =
            pure Account{signers = [signer testGoodPublicKey]}
        | otherwise = throwError err404

    signer key = Signer{key, type_ = Ed25519PublicKey, weight = 1}
