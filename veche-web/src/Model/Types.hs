{-# OPTIONS -Wno-orphans #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Types (
    AccessLevel (..),
    Choice (..),
    CommentType (..),
    Poll (..),
    StellarMultiSigAddress (..),
) where

import ClassyPrelude

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Database.Persist.TH (derivePersistField)
import Stellar.Horizon.Types qualified as Stellar
import Text.Blaze.Html (ToMarkup, toMarkup)
import Web.PathPieces (PathPiece, readFromPathPiece, showToPathPiece)
import Web.PathPieces qualified

import Database.Persist.Extra (JsonString (..))

data Choice = Approve | Reject
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
    = CommentApprove
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
        CommentApprove  -> "approved"
        CommentClose    -> "closed issue"
        CommentEdit     -> "edited issue"
        CommentReject   -> "rejected"
        CommentReopen   -> "reopened issue"
        CommentStart    -> "started issue"
        CommentText     -> ""

deriving newtype instance PersistField    Stellar.Address
deriving newtype instance PersistFieldSql Stellar.Address

newtype StellarMultiSigAddress = StellarMultiSigAddress Stellar.Address
    deriving newtype (PersistField, PersistFieldSql)
    deriving stock Show

-- | Type of poll
data Poll = BySignerWeight
    deriving (Eq, Read, Show)
derivePersistField "Poll"

-- | Levels are listed here in order of increasing security to use
-- 'Ord' instance.
data AccessLevel = AccessLevelUninvolved | AccessLevelHolder | AccessLevelSigner
    deriving (Eq, Ord, Read, Show)
derivePersistField "AccessLevel"
