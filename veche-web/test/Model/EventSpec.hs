{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.EventSpec (spec) where

import Stellar.Horizon.Types qualified as Stellar
import TestImport

import Genesis (mtlFund)
import Model.Event qualified as Event
import Model.Types (EventType (IssueClosed, IssueCreated, IssueReopened))

spec :: Spec
spec =
    withApp do
        it "records an event when an issue created" do
            create

            events <- runDB Event.dbGetUndelivered
            [(type_, issue) | Entity _ Event{type_, issue} <- events]
                === [(IssueCreated, Just issueId)]

        it "records events about closing and reopening" do
            create
            close
            reopen

            events <- runDB Event.dbGetUndelivered
            [(type_, issue) | Entity _ Event{type_, issue} <- events]
                === [ (IssueCreated , Just issueId)
                    , (IssueClosed  , Just issueId)
                    , (IssueReopened, Just issueId)
                    ]
  where
    userIdent = "ff0aca40-ed41-5ab5-8dd4-6dd03ae92ccb"
    issueId = IssueKey 1

    create = do
        userEntity <- createUser userIdent Nothing
        authenticateAs userEntity

        -- allow the user to create a new issue
        runDB $
            insert_
                StellarSigner
                    { target    = mtlFund
                    , key       = Stellar.Address userIdent
                    , weight    = 1
                    }

        get IssueNewR -- get CSRF token
        statusIs 200

        request do
            setMethod "POST"
            setUrl IssuesR
            addTokenFromCookie
            addPostParam "title" "Name road"
            addPostParam "body" "Shirt typical invented. Date flower."
        statusIs 303

    close = do
        postWithCsrf $ IssueCloseR issueId
        statusIs 303

    reopen = do
        postWithCsrf $ IssueReopenR issueId
        statusIs 303
