{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
    ( CommentType (..)
    ) where

import ClassyPrelude.Yesod

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)

data CommentType
    = CommentApprove
    | CommentClose
    | CommentEdit
    | CommentReject
    | CommentReopen
    | CommentRequestInfo
    | CommentStart
    | CommentText
    -- TODO | CommentVote
    deriving (Eq, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_' . drop 7}
    ''CommentType
derivePersistFieldJSON "CommentType"
