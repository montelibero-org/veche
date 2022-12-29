{-# LANGUAGE QuasiQuotes #-}

module Templates.Stellar (renderTx) where

-- prelude
import Import

-- global
import Data.ByteString.Base64 qualified as Base64
-- import Network.Stellar.TransactionXdr (TransactionEnvelope)

renderTx :: TransactionBin -> Widget
renderTx (TransactionBin envelopeXdr) = [whamlet|<code>#{envelopeXdrB64}|]
  where
    envelopeXdrB64 = decodeUtf8Throw $ Base64.encode envelopeXdr
