{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Stellar.Horizon.Types
    ( Account (..)
    , Address (..)
    , Asset (..)
    , Balance (..)
    , Record (..)
    , Records (..)
    , Signer (..)
    , SignerType (..)
    , Transaction (..)
    ) where

import Data.Aeson (FromJSON, ToJSON, camelTo2, constructorTagModifier,
                   defaultOptions, fieldLabelModifier, object, parseJSON,
                   toJSON, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types qualified as Aeson
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Traversable (for)
import Servant.API (FromHttpApiData, ToHttpApiData)

data Account = Account
    { account_id :: Address
        -- ^ This account’s public key encoded in a base32 string
        -- representation.
    , balances :: [Balance] -- ^ The assets this account holds.
    , signers :: [Signer]
        -- ^ The public keys and associated weights that can be used to
        -- authorize transactions for this account.
        -- Used for multi-sig.
    }
    deriving (Show)

newtype Address = Address Text
    deriving newtype
        (Eq, FromHttpApiData, FromJSON, Ord, Show, ToHttpApiData, ToJSON)

data Balance = Balance
    { balance :: Text
        -- ^ The number of units of an asset held by this account.
    , asset_code :: Maybe Text
        -- ^ The code for this asset.
    , asset_issuer :: Maybe Address
        -- ^ The Stellar address of this asset’s issuer.
    }
    deriving (Show)

newtype Asset =
    Asset
        Text -- ^ asset code in format "{code}:{issuer}"
    deriving newtype (FromHttpApiData, ToHttpApiData)

data Record a = Record
    { paging_token  :: Text
    , value         :: a
    }
    deriving (Show)

instance FromJSON a => FromJSON (Record a) where
    parseJSON =
        withObject "Record" \obj -> do
            paging_token <- obj .: "paging_token"
            let valueObj = Aeson.delete "paging_token" obj
            value <- parseJSON $ Aeson.Object valueObj
            pure Record{paging_token, value}

instance ToJSON a => ToJSON (Record a) where
    toJSON Record{paging_token, value} =
        Aeson.Object $
        Aeson.insert "paging_token" (Aeson.String paging_token) $
        assertObject $ toJSON value

assertObject :: Aeson.Value -> Aeson.Object
assertObject = \case
    Aeson.Object obj -> obj
    v -> either error error $ Aeson.parseEither (Aeson.typeMismatch "Object") v

newtype Records a =
    Records
        [Record a] -- ^ _embedded.records
    deriving (Show)

instance FromJSON a => FromJSON (Records a) where
    parseJSON =
        withObject "Horizon response with _embedded.records" \obj -> do
            _embedded <- obj .: "_embedded"
            parseEmbedded _embedded
      where
        parseEmbedded =
            withObject "Records" \obj -> do
                records <- obj .: "records"
                Records <$> for records parseJSON

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

data Transaction = Transaction
    { id :: Text -- ^ A unique identifier for this transaction.
    , successful :: Bool
        -- ^ Indicates if this transaction was successful or not.
    , created_at :: UTCTime -- ^ The date this transaction was created.
    , source_account :: Text -- ^ The account that originates the transaction.
    , memo :: Text -- ^ The optional memo attached to a transaction.
    , memo_type :: Text
        -- ^ The type of memo. Potential values include MEMO_TEXT, MEMO_ID,
        -- MEMO_HASH, MEMO_RETURN.
    }
    deriving Show

concat
    <$> traverse
            (deriveJSON
                defaultOptions
                    { constructorTagModifier    = camelTo2 '_'
                    , fieldLabelModifier        = dropWhileEnd (== '_')
                    })
            [''Account, ''Balance, ''Signer, ''SignerType, ''Transaction]
