{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Issue (selectWithoutVoteFromUser) where

import Import.NoFoundation

import Database.Persist.Sql (rawSql)

import Genesis (mtlFund)

selectWithoutVoteFromUser ::
    PersistSql app => Entity User -> HandlerFor app [Entity Issue]
selectWithoutVoteFromUser (Entity userId User{userStellarAddress}) =
    runDB do
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ ListIssues signerId
        rawSql
            "SELECT ??\
            \ FROM\
                \ issue\
                \ LEFT JOIN\
                \ (SELECT * FROM vote WHERE vote.user = ?) AS vote\
                \ ON issue.id = vote.issue\
            \ WHERE vote.id IS NULL AND issue.open"
            [toPersistValue userId]
