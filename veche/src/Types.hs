{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
    ( CommentType (..)
    ) where

import ClassyPrelude.Yesod

import Data.Aeson (camelTo2, constructorTagModifier, defaultOptions)
import Data.Aeson.TH (deriveJSON)

data CommentType
    = CommentText
    | CommentOpen
    | CommentClose
    | CommentReopen
    | CommentEdit
    -- TODO | CommentVote
    -- TODO | CommentApprove
    -- TODO | CommentReject
    -- TODO | CommentRequestInfo
    deriving (Eq, Show)
deriveJSON
    defaultOptions{constructorTagModifier = camelTo2 '_' . drop 7}
    ''CommentType
derivePersistFieldJSON "CommentType"
