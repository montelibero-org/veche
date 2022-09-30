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
import Model.Types (EventType (IssueCreated))

spec :: Spec
spec =
    withApp do
        it "saves an event when an issue created" do
            prepare

            events <- runDB Event.dbSelectAll
            [(type_, issue) | Event{type_, issue} <- events]
                === [(IssueCreated, Just $ IssueKey 1)]
  where
    userIdent = "ff0aca40-ed41-5ab5-8dd4-6dd03ae92ccb"

    prepare = do
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
