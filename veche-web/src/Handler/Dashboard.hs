{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Dashboard (getDashboardR) where

import Import

import Model.Issue qualified as Issue
import Model.Request qualified as Request
import Templates.Issue (issueRequestTable, issueTable)

getDashboardR :: Handler Html
getDashboardR = do
    user@(Entity userId _) <- requireAuth
    issues <- Issue.selectWithoutVoteFromUser user
    requests <- Request.selectActiveByUser userId
    defaultLayout $(widgetFile "dashboard")
