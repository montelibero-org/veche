{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Model.Request (
    RequestMaterialized (..),
    selectByUser,
) where

import Import

import Database.Persist.Sql (rawSql)

data RequestMaterialized = RequestMaterialized
    { issue   :: Entity Issue
    , comment :: Entity Comment
    }

selectByUser :: UserId -> Handler [RequestMaterialized]
selectByUser userId = do
    requests <-
        runDB $
            rawSql
                @(Entity Issue, Entity Comment)
                "SELECT ??, ??\
                \ FROM request\
                \ JOIN issue ON request.issue = issue.id\
                \ JOIN comment ON request.comment = comment.id\
                \ WHERE request.user = ?"
                [toPersistValue userId]
    pure [RequestMaterialized{issue, comment} | (issue, comment) <- requests]
