{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Dashboard (getDashboardR) where

import Import

import Model.Issue qualified as Issue
import Templates.Issue (issueTable)

getDashboardR :: Handler Html
getDashboardR = do
    user <- requireAuth
    issues <- Issue.selectWithoutVoteFromUser user
    defaultLayout $(widgetFile "dashboard")
