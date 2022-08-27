module Types.Comment where

import Import

data CommentMaterialized = CommentMaterialized
    { id             :: CommentId
    , comment        :: Comment
    , author         :: User
    , requestedUsers :: [User]
    }
    deriving (Show)
