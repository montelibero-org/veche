{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth.StellarSpec (spec) where

import TestImport

-- global
import Control.Concurrent (forkIO, killThread)
import Control.Monad.Except (throwError)
import Data.Text qualified as Text
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant ((:<|>) ((:<|>)))
import Servant.Server (Server, err404, serve)
import Servant.Server qualified as Servant
import Text.Shakespeare.Text (stextFile)
import Text.XML.Cursor (content, descendant)

-- project
import Stellar.Horizon.API (API, api)
import Stellar.Horizon.Types (Account (..), Signer (..),
                              SignerType (Ed25519PublicKey))
import Yesod.Auth.Stellar.Internal (python3)

-- package
import Model.User qualified as User

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

                it "authenticates when correct tx-response (mainnet)" do
                    testAuthenticationOk
                        (testGoodPublicKey, testGoodSecretKey)
                        mainnetPassphrase

                it "authenticates when correct tx-response (testnet)" $
                    testAuthenticationOk
                        (testGoodPublicKey, testGoodSecretKey)
                        testnetPassphrase

                it "doesn't authenticate when incorrect tx-response" do
                    request do
                        setMethod "POST"
                        setUrl $ AuthR $ PluginR "stellar" []
                        addPostParam "response" testGoodTxUnsinged
                    statusIs 400

testAuthenticationOk :: (Text, Text) -> Text -> YesodExample App ()
testAuthenticationOk (address, secretKey) networkPassphrase = do
    get (AuthR LoginR, [("stellar_address", address)])
    statusIs 200

    transactionXdr <-
        the . (content <=< descendant) . parseHTML . the <$>
        htmlQuery ".stellar_challenge"
    response <-
        either (error . Text.unpack) id <$> python3 $(stextFile "test/sign.py")

    request do
        setMethod "POST"
        setUrl $ AuthR $ PluginR "stellar" []
        addPostParam "response" response
        addToken_ "#auth_stellar_response_form"
    printBody
    statusIs 303 -- okay redirect

    get UserR
    statusIs 200 -- authenticated

    users <- runDB User.selectValList
    assertEq
        "users after auth"
        users
        [User{userName = Nothing, userStellarAddress = testGoodPublicKey}]

testGoodPublicKey :: Text
testGoodPublicKey = "GDLNZNS3HM3I3OAQKYV5WBACXCUU7JWEQLGGCAEV7E4LA4BY2XFOLEWX"

testGoodSecretKey :: Text
testGoodSecretKey = "SA3KFUDKK5YDG2X2OKLW26JSTPGPUL6DOODIYZ4A2C4ZI3Y5RG2YJQVW"

testGoodTxUnsinged :: Text
testGoodTxUnsinged =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAA"

withMockHorizon :: IO () -> IO ()
withMockHorizon =
    bracket (forkIO $ Warp.run 9999 horizonTestApp) killThread . const

horizonTestApp :: Wai.Application
horizonTestApp = serve api horizonTestServer
  where

    horizonTestServer :: Server API
    horizonTestServer = getAccount :<|> getAccounts

    getAccount :: Text -> Servant.Handler Account
    getAccount address
        | address == testGoodPublicKey =
            pure
                Account
                { account_id    = address
                , balances      = []
                , paging_token  = address
                , signers       = [signer address]
                }
        | otherwise = throwError err404

    getAccounts _ _ _ = throwError err404

    signer key = Signer{key, type_ = Ed25519PublicKey, weight = 1}

the :: HasCallStack => [a] -> a
the = \case
    [a] -> a
    xs -> error $ "the: " <> show (length xs) <> " items"

mainnetPassphrase :: Text
mainnetPassphrase = "Public Global Stellar Network ; September 2015"

testnetPassphrase :: Text
testnetPassphrase = "Test SDF Network ; September 2015"
