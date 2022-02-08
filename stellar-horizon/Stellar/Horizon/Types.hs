{-# LANGUAGE TemplateHaskell #-}

module Stellar.Horizon.Types
    ( Account (..)
    , Signer (..)
    ) where

import Data.Aeson (defaultOptions, fieldLabelModifier)
import Data.Aeson.TH (deriveJSON)
import Data.List (dropWhileEnd)
import Data.Text (Text)

newtype Account = Account{signers :: [Signer]}
    deriving (Show)

data Signer = Signer{weight :: Integer, key :: Text, type_ :: Text}
    deriving (Show)

concat
    <$> traverse
            (deriveJSON
                defaultOptions{fieldLabelModifier = dropWhileEnd (== '_')})
            [''Account, ''Signer]
