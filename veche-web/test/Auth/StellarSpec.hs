{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.StellarSpec (spec) where

import TestImport

-- global
import Control.Concurrent (forkIO, killThread)
import Control.Monad.Except (throwError)
import Data.ByteString.Base64 qualified as Base64
import Network.ONCRPC.XDR (xdrDeserialize, xdrSerialize)
import Network.Stellar.Builder qualified as Stellar
import Network.Stellar.Keypair qualified as Stellar
import Network.Stellar.Network qualified as Stellar
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant ((:<|>) ((:<|>)))
import Servant.Server (Server, err404, serve)
import Servant.Server qualified as Servant
import Test.HUnit (assertFailure)
import Text.XML.Cursor (content, descendant)

-- project
import Stellar.Horizon.API (API, api)
import Stellar.Horizon.Types (Account (Account), Signer (Signer),
                              SignerType (Ed25519PublicKey))
import Stellar.Horizon.Types qualified as Stellar

-- package
import Model.User (User (User))
import Model.User qualified as User

spec :: Spec
spec =
    around_ withMockHorizon $
    withApp $
    describe "Auth.Stellar" do

        describe "initial public key form" $

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

            it "authenticates when correct tx-response (public network)" $
                testAuthenticationOk testGoodKeyPair Stellar.publicNetwork

            it "authenticates when correct tx-response (test network)" $
                testAuthenticationOk testGoodKeyPair Stellar.testNetwork

            it "doesn't authenticate when incorrect tx-response" do
                request do
                    setMethod "POST"
                    setUrl $ AuthR $ PluginR "stellar" []
                    addPostParam "response" testGoodTxUnsinged
                statusIs 400

testAuthenticationOk ::
    Stellar.KeyPair -> Stellar.Network -> YesodExample App ()
testAuthenticationOk keyPair network = do
    get (AuthR LoginR, [("stellar_address", address)])
    statusIs 200

    envelopeXdrBase64 <-
        the . (content <=< descendant) . parseHTML . the <$>
        htmlQuery ".stellar_challenge"
    envelopeXdrRaw <- assertRight $ Base64.decode $ encodeUtf8 envelopeXdrBase64
    envelope <- assertRight $ xdrDeserialize envelopeXdrRaw
    envelopeSigned <- assertRightShow $ Stellar.sign network envelope [keyPair]
    let envelopeSignedXdrBase64 =
            decodeUtf8Throw $ Base64.encode $ xdrSerialize envelopeSigned

    request do
        setMethod "POST"
        setUrl $ AuthR $ PluginR "stellar" []
        addPostParam "response" envelopeSignedXdrBase64
        addToken_ "#auth_stellar_response_form"
    printBody
    statusIs 303 -- okay redirect

    get UserR
    statusIs 200 -- authenticated

    users <- runDB User.dbSelectAll
    assertEq
        "users after auth"
        [ User
            { name              = Nothing
            , notifyIssueAdded  = False
            , stellarAddress    = Stellar.Address address
            }
        ]
        users

  where
    Stellar.KeyPair{kpPublicKey} = keyPair
    address = Stellar.encodePublicKey kpPublicKey

testGoodPublicKey :: Text
testGoodPublicKey = "GDLNZNS3HM3I3OAQKYV5WBACXCUU7JWEQLGGCAEV7E4LA4BY2XFOLEWX"

testGoodKeyPair :: Stellar.KeyPair
testGoodKeyPair =
    Stellar.fromPrivateKey'
        "SA3KFUDKK5YDG2X2OKLW26JSTPGPUL6DOODIYZ4A2C4ZI3Y5RG2YJQVW"

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

    getAccount :: Stellar.Address -> Servant.Handler Account
    getAccount account_id
        | account_id == Stellar.Address testGoodPublicKey =
            pure Account{account_id, balances, paging_token, signers}
        | otherwise = throwError err404
      where
        balances = []
        Stellar.Address paging_token = account_id
        signers = [signer account_id]

    getAccounts _ _ _ = throwError err404

    signer (Stellar.Address key) =
        Signer{key, type_ = Ed25519PublicKey, weight = 1}

the :: HasCallStack => [a] -> a
the = \case
    [a] -> a
    xs -> error $ "the: " <> show (length xs) <> " items"

assertRightShow :: (HasCallStack, Show e) => Either e a -> YesodExample App a
assertRightShow = either (liftIO . assertFailure . show) pure

assertRight :: HasCallStack => Either String a -> YesodExample App a
assertRight = either (liftIO . assertFailure) pure
