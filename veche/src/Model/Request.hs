{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Request (selectByUser) where

import Import

import Database.Persist.Sql (rawSql)

selectByUser :: UserId -> Handler [Entity Comment]
selectByUser userId =
    runDB $
    rawSql
        "SELECT ??\
            \ FROM comment, request ON comment.id = request.comment\
            \ WHERE request.user = ?"
        [toPersistValue userId]
