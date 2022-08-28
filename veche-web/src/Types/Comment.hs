module Types.Comment where

import Import

data CommentMaterialized = CommentMaterialized
    { id             :: CommentId
    , comment        :: Comment
    , author         :: Entity User
    , requestedUsers :: [User]
    }
    deriving (Show)
