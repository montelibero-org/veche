module Types.Comment where

import Import

data CommentMaterialized = CommentMaterialized
    { comment   :: Comment
    , author    :: User
    }
