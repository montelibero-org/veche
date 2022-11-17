{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | https://core.telegram.org/widgets/login#receiving-authorization-data
module Telegram.Auth (AuthorizationData (..), receiveAuthorizationData) where

-- prelude
import Foundation.Base
import Import.NoFoundation hiding (hash)

-- global
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Network.Wai (queryString)
import Yesod.Core (HandlerSite, RenderMessage, waiRequest)
import Yesod.Form (FormInput, FormMessage, runInputGet)

data AuthorizationData = AuthorizationData
    { id        :: Int64
    , username  :: Text
    }

authorizationDataForm ::
    (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
    FormInput m AuthorizationData
authorizationDataForm = do
    id          <- ireq intField    "id"
    username    <- ireq textField   "username"
    pure AuthorizationData{..}

verifyAuthorizationData :: MonadHandler m => ByteString -> m ()
verifyAuthorizationData botToken = do
    params <- queryString <$> waiRequest
    hash <-
        case lookup "hash" params of
            Just (Just hash)    -> pure hash
            _                   -> invalidArgs ["parameter `hash` must present"]
    let dataCheckString =
            intercalate
                "\n"
                [ key <> "=" <> value
                | (key, Just value) <- sort params, key /= "hash"
                ]
    let hash' = SHA256.hmac secretKey dataCheckString
    when (Base16.encode hash' /= hash) $
        invalidArgs ["authorization data hash mismatch"]
  where
    secretKey = SHA256.hash botToken

receiveAuthorizationData ::
    (MonadHandler m, HandlerSite m ~ App) => m AuthorizationData
receiveAuthorizationData = do
    App{appSettings = AppSettings{appTelegramBotToken}} <- getYesod
    ad <- runInputGet authorizationDataForm
    verifyAuthorizationData $ encodeUtf8 appTelegramBotToken
    pure ad
