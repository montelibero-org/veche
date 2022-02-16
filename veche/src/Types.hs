{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Types (
    Choice (..),
    CommentType (..),
) where

import ClassyPrelude.Yesod

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)

data Choice = Approve | Reject
    deriving (Eq, Ord, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_' . drop 7}
    ''Choice
derivePersistFieldJSON "Choice"

data CommentType
    = CommentApprove
    | CommentClose
    | CommentEdit
    | CommentReject
    | CommentReopen
    | CommentRequestInfo
    | CommentStart
    | CommentText
    deriving (Eq, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_'}
    ''CommentType
derivePersistFieldJSON "CommentType"
