{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Stellar.Horizon.Types
    ( Account (..)
    , Asset (..)
    , Balance (..)
    , Records (..)
    , Signer (..)
    , SignerType (..)
    ) where

import Data.Aeson (FromJSON, ToJSON, camelTo2, constructorTagModifier,
                   defaultOptions, fieldLabelModifier, object, withObject, (.:),
                   (.=))
import Data.Aeson qualified
import Data.Aeson.TH (deriveJSON)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Servant.API (FromHttpApiData, ToHttpApiData)

data Account = Account
    { account_id :: Text
        -- ^ This account’s public key encoded in a base32 string
        -- representation.
    , balances :: [Balance] -- ^ The assets this account holds.
    , paging_token :: Text
    , signers :: [Signer]
        -- ^ The public keys and associated weights that can be used to
        -- authorize transactions for this account.
        -- Used for multi-sig.
    }
    deriving (Show)

data Balance = Balance
    { balance :: Text
        -- ^ The number of units of an asset held by this account.
    , asset_code :: Maybe Text
        -- ^ The code for this asset.
    , asset_issuer :: Maybe Text
        -- ^ The Stellar address of this asset’s issuer.
    }
    deriving (Show)

newtype Asset =
    Asset
        Text -- ^ asset code in format "{code}:{issuer}"
    deriving newtype (FromHttpApiData, ToHttpApiData)

newtype Records a =
    Records
        [a] -- ^ _embedded.records
    deriving (Show)

instance FromJSON a => FromJSON (Records a) where
    parseJSON =
        withObject "Horizon response with _embedded.records" \obj -> do
            _embedded <- obj .: "_embedded"
            parse1 _embedded
      where
        parse1 = withObject "Records" \obj -> Records <$> obj .: "records"

instance ToJSON a => ToJSON (Records a) where
    toJSON (Records records) =
        object ["_embedded" .= object ["records" .= records]]

data Signer = Signer
    { weight :: Int
        -- ^ The numerical weight of a signer. Used to determine if a
        -- transaction meets the `threshold` requirements.
    , key :: Text
        -- ^ A hash of characters dependent on the signer type.
    , type_ :: SignerType
        -- ^ The type of hash for this signer.
    }
    deriving (Show)

data SignerType
    = Ed25519PublicKey  -- ^ A normal Stellar public key.
    | Sha256Hash
        -- ^ The SHA256 hash of some arbitrary `x`.
        -- Adding a signature of this type allows anyone who knows x to sign a
        -- transaction from this account.
        -- _Note: Once this transaction is broadcast, x will be known publicly._
    | PreauthTx
    deriving (Show)

concat
    <$> traverse
            (deriveJSON
                defaultOptions
                    { constructorTagModifier    = camelTo2 '_'
                    , fieldLabelModifier        = dropWhileEnd (== '_')
                    })
            [''Account, ''Balance, ''Signer, ''SignerType]
