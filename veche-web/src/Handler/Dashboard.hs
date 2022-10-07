{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Dashboard (getDashboardR) where

import Import

import Model.Issue qualified as Issue
import Model.Request qualified as Request
import Model.User qualified as User
import Templates.Issue (issueRequestTable, issueTable)

getDashboardR :: Handler Html
getDashboardR = do
    userE@(Entity userId user) <- requireAuth
    isSigner <- User.isSigner user
    issuesToVote <-
        if isSigner then do
            issues <- Issue.selectWithoutVoteFromUser userE
            pure
                [whamlet|
                    <h2>Voting required (#{show $ length issues})
                    ^{issueTable issues}
                |]
        else
            pure mempty
    requests <- Request.selectActiveByUser userId
    defaultLayout $(widgetFile "dashboard")
