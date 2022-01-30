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

testPublicKey :: Text
testPublicKey = "GDLNZNS3HM3I3OAQKYV5WBACXCUU7JWEQLGGCAEV7E4LA4BY2XFOLEWX"

-- testSecretKey = "SA3KFUDKK5YDG2X2OKLW26JSTPGPUL6DOODIYZ4A2C4ZI3Y5RG2YJQVW"
