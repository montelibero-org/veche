{-# LANGUAGE TemplateHaskell #-}

module Stellar.Horizon.Types
    ( Account (..)
    , Signer (..)
    , SignerType (..)
    ) where

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions,
                   fieldLabelModifier)
import Data.Aeson.TH (deriveJSON)
import Data.List (dropWhileEnd)
import Data.Text (Text)

newtype Account = Account{signers :: [Signer]}
    deriving (Show)

data Signer = Signer{weight :: Int, key :: Text, type_ :: SignerType}
    deriving (Show)

data SignerType
    = Ed25519PublicKey
    | Sha256Hash
    | PreauthTx
    deriving (Show)

concat
    <$> traverse
            (deriveJSON
                defaultOptions
                    { constructorTagModifier    = camelTo2 '_'
                    , fieldLabelModifier        = dropWhileEnd (== '_')
                    })
            [''Account, ''Signer, ''SignerType]
