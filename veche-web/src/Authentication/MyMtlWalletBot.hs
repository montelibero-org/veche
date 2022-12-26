{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication.MyMtlWalletBot (authMyMtlWalletBot) where

-- prelude
import Foundation.Base
import Import.NoFoundation hiding (ap)

-- global
import Data.ByteString.Base64 qualified as Base64
import Data.Text qualified as Text
import Network.Stellar.Keypair (decodePublicKey)
import Network.Stellar.Signature (verifyBlob)
import Yesod.Core (badMethod, liftHandler, sendResponse)
import Yesod.Form (runInputGet)

-- project
import Stellar.Simple qualified as Stellar

-- component
import Model.Verifier qualified as Verifier

type Method = Text

type Piece = Text

pluginName :: Text
pluginName = "mymtlwalletbot"

authMyMtlWalletBot :: AuthPlugin App
authMyMtlWalletBot =
    AuthPlugin{apName = pluginName, apLogin = login, apDispatch = dispatch}

login :: (Route Auth -> Route App) -> Widget
login _ = do
    nonce <- liftHandler Verifier.getKeyNoAddress
    [whamlet|
        <a .btn.btn-primary .mymtlwalletbot-authn-start
                href="https://t.me/MyMTLWalletBot?start=veche_#{nonce}"
                role=button>
            _{MsgLogInViaMMWB}
    |]

data AuthnParams = AuthnParams
    { account       :: Text
    , signature_    :: Text
    , verifier      :: Maybe Text
        -- ^ Nonce from request.
        -- If present, is checked against the database and the account.
        -- If not, nonce is got from session.
    }

getAuthnParams :: Handler AuthnParams
getAuthnParams =
    runInputGet do
        account     <- ireq textField "account"
        signature_  <- ireq textField "signature"
        verifier    <- iopt textField "verifier"
        pure AuthnParams{..}

base64Decode :: MonadHandler m => Text -> m ByteString
base64Decode =
    either (invalidArgs . pure . Text.pack) pure . Base64.decode . encodeUtf8

getAuthenticatedAccount :: Handler Text
getAuthenticatedAccount = do
    ap <- getAuthnParams
    signatureBs <- base64Decode ap.signature_
    nonce <-
        case ap.verifier of
            Just nonce -> do
                nonceOk <-
                    Verifier.checkAndRemoveKey
                        (Stellar.Address ap.account)
                        nonce
                unless nonceOk $ invalidArgs ["Bad verifier"]
                pure nonce
            Nothing -> do
                -- take nonce from session
                mNonce <- Verifier.getAndRemoveKey
                mNonce ?| invalidArgs ["Bad verifier"]
    let message = encodeUtf8 $ ap.account <> nonce
    account' <- decodePublicKey ap.account ?| invalidArgs ["Bad public key"]
    unless (verifyBlob account' message signatureBs) $
        invalidArgs ["Bad signature"]
    pure ap.account

dispatch :: Method -> [Piece] -> AuthHandler App TypedContent
dispatch method path =
    case path of
        -- authentication
        [] -> do
            unless (method == "GET") badMethod
            account <- liftHandler getAuthenticatedAccount
            setCredsRedirect $ mkCreds account
        -- make new nonce
        ["verifier"] -> do
            unless (method == "POST") badMethod
            address <- runInputGet $ ireq textField "account"
            stellarAddress <- validateAddress address
            nonce <- liftHandler $ Verifier.getKey stellarAddress
            sendResponse nonce
        _ -> notFound
  where

    mkCreds credsIdent =
        Creds{credsPlugin = pluginName, credsIdent, credsExtra = []}

    validateAddress text
        | Just _ <- decodePublicKey text = pure $ Stellar.Address text
        | otherwise                      = invalidArgs ["Bad account"]
