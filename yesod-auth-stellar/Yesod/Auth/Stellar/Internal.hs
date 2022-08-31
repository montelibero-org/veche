{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Yesod.Auth.Stellar.Internal where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified

decodeUtf8Throw :: ByteString -> Text
decodeUtf8Throw = Data.Text.Encoding.decodeUtf8
