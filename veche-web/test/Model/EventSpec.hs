{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.EventSpec (spec) where

import TestImport

import Model.Event (SomeEvent (SomeEvent))
import Model.Event qualified as Event
import Model.Forum (Forum (Forum), Key (ForumKey))
import Model.Forum qualified
import Model.Issue (Key (IssueKey))
import Model.Types (AccessLevel (AccessLevelUninvolved))

spec :: Spec
spec =
    withApp do
        it "records an event when an issue created" do
            create

            msgs <- getMessages
            msgs === ["A new discussion is started /issues/1"]

        it "records events about closing and reopening" do
            create
            addComment

            msgs <- getMessages
            msgs
                === [ "A new discussion is started /issues/1"
                    , "*2ccb replied to you /issues/1#comment1"
                    , "*2ccb requested you to comment on /issues/1#comment1"
                    ]
  where
    userName = "ff0aca40-ed41-5ab5-8dd4-6dd03ae92ccb"
    forumId = ForumKey "FELLOW"
    issueId = IssueKey 1

    create = do
        userEntity <- createUser userName Nothing
        authenticateAs userEntity

        runDB $
            -- create the forum
            insertKey
                forumId
                Forum
                    { title                 = "Fellow forum"
                    , accessIssueRead       = AccessLevelUninvolved
                    , accessIssueWrite      = AccessLevelUninvolved
                    , accessIssueComment    = AccessLevelUninvolved
                    }

        get $ ForumIssueNewR forumId -- get CSRF token
        statusIs 200

        request do
            setMethod "POST"
            setUrl $ ForumIssuesR forumId
            addRequestHeader ("Accept", "text/plain")
            addTokenFromCookie
            addPostParam "title" "Name road"
            addPostParam "body" "Shirt typical invented. Date flower."
            addPostParam "poll" "1"
        statusIs 303

    addComment = do
        get $ IssueR issueId
        statusIs 200

        request do
            setMethod "POST"
            setUrl CommentsR
            addToken
            addPostParam "issue" $ toPathPiece issueId
            addPostParam "message" "addition swimming center subject"
            addPostParam "request_user" "1"
        printBody
        statusIs 303

    getMessages = do
        events <- runDB Event.dbGetUndelivered
        app <- getTestYesod
        let makeMessage' :: SomeEvent -> YesodExample App Text
            makeMessage' (SomeEvent e) = runDB $ Event.makeMessage app e
        traverse makeMessage' events
