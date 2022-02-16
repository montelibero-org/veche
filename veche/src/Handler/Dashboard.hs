{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Dashboard (getDashboardR) where

import Import

import Genesis (mtlFund)

getDashboardR :: Handler Html
getDashboardR = do
    (_, User{userStellarAddress}) <- requireAuthPair
    issues <-
        runDB do
            Entity signerId _ <-
                getBy403 $ UniqueSigner mtlFund userStellarAddress
            requireAuthz $ ListIssues signerId
            selectList [IssueOpen ==. True] []
    defaultLayout $(widgetFile "dashboard")
