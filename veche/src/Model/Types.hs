{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Types (
    Choice (..),
    CommentType (..),
) where

import ClassyPrelude.Yesod

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Text.Blaze.Html (ToMarkup, toMarkup)

data Choice = Approve | Reject
    deriving (Eq, Ord, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_'}
    ''Choice
derivePersistFieldJSON "Choice"

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
    deriving (Eq, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_' . drop 7}
    ''CommentType
derivePersistFieldJSON "CommentType"
