{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.EventSpec (spec) where

import Stellar.Horizon.Types qualified as Stellar
import TestImport

import Genesis (mtlFund)
import Model.Event (SomeEvent (SomeEvent))
import Model.Event qualified as Event

spec :: Spec
spec =
    withApp do
        it "records an event when an issue created" do
            create

            msgs <- getMessages
            msgs === ["A new discussion is started /issues/1"]

        it "records events about closing and reopening" do
            create
            close
            reopen

            msgs <- getMessages
            msgs
                === [ "A new discussion is started /issues/1"
                    , "Discussion is closed /issues/1"
                    , "Discussion is reopened /issues/1"
                    ]
  where
    userName = "ff0aca40-ed41-5ab5-8dd4-6dd03ae92ccb"
    issueId = IssueKey 1

    create = do
        userEntity <- createUser userName Nothing
        authenticateAs userEntity

        -- allow the user to create a new issue
        runDB $
            insert_
                StellarSigner
                    { target    = mtlFund
                    , key       = Stellar.Address userName
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
            addPostParam "poll" "1"
        statusIs 303

    close = do
        postWithCsrf $ IssueCloseR issueId
        statusIs 303

    reopen = do
        postWithCsrf $ IssueReopenR issueId
        statusIs 303

    getMessages = do
        events <- runDB Event.dbGetUndelivered
        app <- getTestYesod
        let make :: SomeEvent -> Text
            make (SomeEvent e) = Event.makeMessage app e
        pure $ map make events
