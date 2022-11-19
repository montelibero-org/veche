{-# OPTIONS -Wno-orphans #-}

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
) where

import ClassyPrelude

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)
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
    deriving stock (Eq, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_' . drop 7}
    ''CommentType
deriving via JsonString CommentType instance PersistField    CommentType
deriving via JsonString CommentType instance PersistFieldSql CommentType

instance ToMarkup CommentType where
    toMarkup = \case
        CommentAbstain  -> "abstained"
        CommentApprove  -> "approved"
        CommentClose    -> "closed issue"
        CommentEdit     -> "edited issue"
        CommentReject   -> "voted against"
        CommentReopen   -> "reopened issue"
        CommentStart    -> "started issue"
        CommentText     -> ""

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
    deriving (Bounded, Enum, Eq, Read, Show)
derivePersistField "Poll"

data Role
    = Admin
    | Audit
    | HolderOfFcm
    | HolderOfVeche
    | MtlHolder
    | MtlSigner
    deriving (Eq, Ord, Read, Show)
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
    { title             :: Text
    , requireRole       :: Maybe Role
    , enablePoll        :: Bool
    , enableContacts    :: Bool
    , enablePriceOffer  :: Bool
    }
    deriving (Show)

newtype ForumId = ForumKey Text
    deriving newtype
        (Eq, Ord, PathPiece, PersistField, PersistFieldSql, Read, Show)

type EntityForum = (ForumId, Forum)
