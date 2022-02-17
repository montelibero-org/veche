{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Dashboard (getDashboardR) where

import Import

-- global
import Database.Persist.Sql (rawSql)

-- component
import Genesis (mtlFund)

getDashboardR :: Handler Html
getDashboardR = do
    (userId, User{userStellarAddress}) <- requireAuthPair
    issues <-
        runDB do
            Entity signerId _ <-
                getBy403 $ UniqueMember mtlFund userStellarAddress
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
    defaultLayout $(widgetFile "dashboard")
