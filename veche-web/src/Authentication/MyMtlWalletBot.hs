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
import Network.HTTP.Types (urlEncode)
import Network.Stellar.Keypair (decodePublicKey)
import Network.Stellar.Signature (verifyBlob)
import Yesod.Core (liftHandler)
import Yesod.Form (runInputGet)

-- component
import Model.Verifier qualified as Verifier

type Method = Text

type Piece = Text

pluginName :: Text
pluginName = "mymtlwalletbot"

authMyMtlWalletBot :: AuthPlugin App
authMyMtlWalletBot =
    AuthPlugin{apName = pluginName, apLogin = login, apDispatch = dispatch}

toQueryParam :: Text -> Text
toQueryParam = decodeUtf8 . urlEncode True . encodeUtf8

login :: (Route Auth -> Route App) -> Widget
login _ = do
    nonce <- liftHandler Verifier.getKeyNoAddress
    let query = "login_to_veche?nonce=" <> nonce
    [whamlet|
        <a .btn.btn-primary
                href="https://t.me/MyMTLWalletBot?start=#{toQueryParam query}"
                role=button>
            _{MsgLoginViaMMWB}
    |]

data AuthnParams = AuthnParams
    { account   :: Text
    , signature :: Text
    }

base64Decode :: MonadHandler m => Text -> m ByteString
base64Decode =
    either (invalidArgs . pure . Text.pack) pure . Base64.decode . encodeUtf8

getAuthenticatedAccount :: AuthHandler App Text
getAuthenticatedAccount = do
    AuthnParams{account, signature} <-
        runInputGet do
            account   <- ireq textField "account"
            signature <- ireq textField "signature"
            pure AuthnParams{..}
    signatureBs <- base64Decode signature
    mNonce <- liftHandler Verifier.getAndRemoveKey
    nonce <- mNonce ?| invalidArgs ["Bad nonce"]
    let message = encodeUtf8 $ account <> nonce
    account' <- decodePublicKey account ?| invalidArgs ["Bad public key"]
    unless (verifyBlob account' message signatureBs) $
        invalidArgs ["Bad signature"]
    pure account

dispatch :: Method -> [Piece] -> AuthHandler App TypedContent
dispatch _ _ = do
    account <- getAuthenticatedAccount
    setCredsRedirect
        Creds{credsPlugin = pluginName, credsIdent = account, credsExtra = []}
