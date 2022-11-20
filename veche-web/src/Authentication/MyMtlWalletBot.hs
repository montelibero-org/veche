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
import Network.Stellar.Keypair (PublicKey, decodePublicKey, decodePublicKey')
import Network.Stellar.Signature (verifyBlob)
import Yesod.Form (runInputGet)

type Method = Text

type Piece = Text

walletPublicKey :: PublicKey
walletPublicKey =
    decodePublicKey' "GDCGYX7AXIN3EWIBFZ3AMMZU4IUWS4CIZ7Z7VX76WVOIJORCKDDRSIGN"

pluginName :: Text
pluginName = "mymtlwalletbot"

authMyMtlWalletBot :: AuthPlugin App
authMyMtlWalletBot =
    AuthPlugin{apName = pluginName, apLogin = login, apDispatch = dispatch}

login :: (Route Auth -> Route App) -> Widget
login _ =
    [whamlet|
        <a .btn.btn-primary
                href="https://t.me/MyMTLWalletBot?start=login_to_veche"
                role=button>
            _{MsgLoginViaMMWB}
    |]

data AuthnParams = AuthnParams
    { signature_user    :: Text
    , signature_wallet  :: Text
    , stellar_account   :: Text
    , telegram_id       :: Text
    , telegram_username :: Maybe Text
    }

base64Decode :: MonadHandler m => Text -> m ByteString
base64Decode =
    either (invalidArgs . pure . Text.pack) pure . Base64.decode . encodeUtf8

getAuthnParams :: AuthHandler App AuthnParams
getAuthnParams = do
    ap <-
        runInputGet do
            signature_user    <- ireq textField "signature_user"
            signature_wallet  <- ireq textField "signature_wallet"
            stellar_account   <- ireq textField "account"
            telegram_id       <- ireq textField "id"
            telegram_username <- iopt textField "username"
            pure AuthnParams{..}
    userSignature   <- base64Decode ap.signature_user
    walletSignature <- base64Decode ap.signature_wallet
    let message =
            encodeUtf8 $
            intercalate
                "\n"
                (   ["account=" <> ap.stellar_account, "id=" <> ap.telegram_id]
                ++  ["username=" <> u | u <- toList ap.telegram_username]
                )
    account <-
        decodePublicKey ap.stellar_account ?| invalidArgs ["Bad public key"]
    unless (verifyBlob account message userSignature) $
        invalidArgs ["Bad user signature"]
    unless (verifyBlob walletPublicKey message walletSignature) $
        invalidArgs ["Bad wallet signature"]
    pure ap

dispatch :: Method -> [Piece] -> AuthHandler App TypedContent
dispatch _ _ = do
    AuthnParams{stellar_account, telegram_id, telegram_username} <-
        getAuthnParams
    setCredsRedirect
        Creds
        { credsPlugin   = pluginName
        , credsIdent    = stellar_account
        , credsExtra    =
            ("telegram_id", telegram_id)
            : [("telegram_username", u) | u <- toList telegram_username]
        }
