{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.StellarSpec (spec) where

import TestImport

spec :: Spec
spec = withApp do
    describe "Auth.Stellar" do

        describe "initial public key form" do

            it "shows public key form" do
                get $ AuthR LoginR
                statusIs 200
                htmlCount "input[name=stellar_address]" 1

        describe "challenge/response form" do

            it "shows challenge for address" do
                get (AuthR LoginR, [("stellar_address", testPublicKey)])
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
                testAuthenticationOk testTxSignedForMainnet

            it "authenticates when correct tx-response (testnet)" $
                testAuthenticationOk testTxSignedForTestnet

            it "doesn't authenticate when incorrect tx-response" do
                request do
                    setMethod "POST"
                    setUrl $ AuthR $ PluginR "stellar" []
                    addPostParam "response" testUnsingedTx
                statusIs 400

testAuthenticationOk :: Text -> YesodExample App ()
testAuthenticationOk tx = do
    request do
        setMethod "POST"
        setUrl $ AuthR $ PluginR "stellar" []
        addPostParam "response" tx
    statusIs 303 -- okay redirect

    get UserR
    statusIs 200 -- authenticated

    users <- runDB $ fmap entityVal <$> selectList [] []
    assertEq
        "users after auth"
        users
        [ User
            { userName = Nothing
            , userStellarAddress = testPublicKey
            }
        ]

testPublicKey :: Text
testPublicKey = "GDLNZNS3HM3I3OAQKYV5WBACXCUU7JWEQLGGCAEV7E4LA4BY2XFOLEWX"

-- testSecretKey = "SA3KFUDKK5YDG2X2OKLW26JSTPGPUL6DOODIYZ4A2C4ZI3Y5RG2YJQVW"

testUnsingedTx :: Text
testUnsingedTx =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAA"

testTxSignedForMainnet :: Text
testTxSignedForMainnet =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAB\
    \ONXK5QAAAEAy13Yo6c30JzWbUp24b43kCtUitlwbtijOhmU0H6cZlFm8xCa2Y9Eu\
    \WvCG0Rima5rIXHTJnPKHJdI6boKpODwM"

testTxSignedForTestnet :: Text
testTxSignedForTestnet =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAB\
    \ONXK5QAAAEDxIVAlHINvvv9/BcFP6kvXd0Tw/F1w8jEFxVf8UISvDxw6SOmt3Z6B\
    \Nm/CMBwyBtYXYooKhoKW6ky8A0hStCMH"
