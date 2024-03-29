{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Types (
    Choice (..),
    CommentType (..),
    EntityForum,
    Forum (..),
    ForumId (..),
    Poll (..),
    StellarMultiSigAddress (..),
    Role (..),
    Roles,
    TransactionB64 (..),
    TransactionBin (..),
    decodeTxBase64,
    encodeTxBase64,
) where

-- prelude
import ClassyPrelude
import Import.Extra

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, camelTo2,
                   constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Base64 qualified as Base64
import Data.Text qualified as Text
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Database.Persist.TH (derivePersistField)
import Stellar.Horizon.Client qualified as Stellar
import Text.Blaze.Html (ToMarkup, toMarkup)
import Text.Read (readEither)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.HttpApiData qualified
import Web.PathPieces (PathPiece, readFromPathPiece, showToPathPiece)
import Web.PathPieces qualified

import Database.Persist.Extra (JsonString (..))

data Choice = Approve | Reject | Abstain
    deriving (Eq, Ord, Read, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_'}
    ''Choice
deriving via JsonString Choice instance PersistField    Choice
deriving via JsonString Choice instance PersistFieldSql Choice

instance PathPiece Choice where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

instance ToMarkup Choice where
    toMarkup = toMarkup . show

data CommentType
    = CommentAbstain
    | CommentApprove
    | CommentClose
    | CommentEdit
    | CommentReject
    | CommentReopen
    | CommentStart
    | CommentText
    | CommentTombstone
    deriving stock (Eq, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_' . drop 7}
    ''CommentType
deriving via JsonString CommentType instance PersistField    CommentType
deriving via JsonString CommentType instance PersistFieldSql CommentType

instance ToMarkup CommentType where
    toMarkup = \case
        CommentAbstain      -> "abstained"
        CommentApprove      -> "approved"
        CommentClose        -> "closed issue"
        CommentEdit         -> "edited issue"
        CommentReject       -> "voted against"
        CommentReopen       -> "reopened issue"
        CommentStart        -> "started issue"
        CommentText         -> ""
        CommentTombstone    -> "deleted message"

deriving newtype instance PersistField    Stellar.Address
deriving newtype instance PersistFieldSql Stellar.Address

newtype StellarMultiSigAddress = StellarMultiSigAddress Stellar.Address
    deriving newtype (PersistField, PersistFieldSql)
    deriving stock Show

-- | Type of poll
data Poll
    = ByAmountOfFcm
    | ByAmountOfVeche
    | ByMtlAmount
    | BySignerWeight
    deriving (Bounded, Enum, Eq, FromJSON, Generic, Read, Show, ToJSON)
derivePersistField "Poll"

data Role
    = Admin
    | Audit
    | HolderOfFcm
    | HolderOfVeche
    | MtlHolder
    | MtlSigner
    deriving (Eq, Generic, Ord, Read, Show)
deriveJSON defaultOptions ''Role
derivePersistField "Role"

instance FromHttpApiData Role where
    parseUrlPiece = first Text.pack . readEither . Text.unpack

instance ToHttpApiData Role where
    toUrlPiece = tshow

instance PathPiece Role where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

type Roles = Set Role

data Forum = Forum
    { enableAttachTx    :: Bool
    , enableContacts    :: Bool
    , enablePriceOffer  :: Bool
    , pollOptions       :: [Poll]
    , requireRole       :: Maybe Role
    , title             :: Text
    }
    deriving (Generic, Show, ToJSON)

newtype ForumId = ForumKey Text
    deriving newtype
        ( Eq
        , FromHttpApiData
        , FromJSON
        , Ord
        , PathPiece
        , PersistField
        , PersistFieldSql
        , Read
        , Show
        , ToJSON
        , ToJSONKey
        )
    deriving stock (Generic)

type EntityForum = (ForumId, Forum)

-- | Transaction binary. For Stellar, it must be XDR of TransactionEnvelope.
newtype TransactionBin = TransactionBin ByteString
    deriving newtype (Eq, PersistField, PersistFieldSql)
    deriving stock (Show)

-- | Transaction binary encoded as Base64.
-- Must be Base64 equivalent of 'TransactionBin'.
newtype TransactionB64 = TransactionB64 Text
    deriving stock (Eq, Show)

encodeTxBase64 :: TransactionBin -> TransactionB64
encodeTxBase64 =
    TransactionB64 . decodeUtf8Throw . Base64.encode . unwrap TransactionBin

decodeTxBase64 :: TransactionB64 -> Either String TransactionBin
decodeTxBase64 =
    fmap TransactionBin . Base64.decode . encodeUtf8 . unwrap TransactionB64
