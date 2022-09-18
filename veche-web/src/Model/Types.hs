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
    Choice (..),
    CommentType (..),
    StellarMultiSigAddress (..),
) where

import ClassyPrelude

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Stellar.Horizon.Types qualified as Stellar
import Text.Blaze.Html (ToMarkup, toMarkup)

import Database.Persist.Extra (JsonString (..))

data Choice = Approve | Reject
    deriving (Eq, Ord, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_'}
    ''Choice
deriving via JsonString Choice instance PersistField    Choice
deriving via JsonString Choice instance PersistFieldSql Choice

instance ToMarkup Choice where
    toMarkup = toMarkup . show

data CommentType
    = CommentApprove
    | CommentClose
    | CommentEdit
    | CommentReject
    | CommentReopen
    | CommentRequestInfo
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
        CommentApprove      -> "approved"
        CommentClose        -> "closed issue"
        CommentEdit         -> "edited issue"
        CommentReject       -> "rejected"
        CommentReopen       -> "reopened issue"
        CommentRequestInfo  -> "requested additional information"
        CommentStart        -> "started issue"
        CommentText         -> ""

deriving newtype instance PersistField    Stellar.Address
deriving newtype instance PersistFieldSql Stellar.Address

newtype StellarMultiSigAddress = StellarMultiSigAddress Stellar.Address
    deriving newtype (PersistField, PersistFieldSql)
    deriving stock Show
