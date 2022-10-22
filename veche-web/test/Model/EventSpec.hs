{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.EventSpec (spec) where

import TestImport

import Model.Event (SomeEvent (SomeEvent))
import Model.Event qualified as Event
import Model.Forum (ForumId (ForumKey))
import Model.Issue (Key (IssueKey))
import Text.Blaze.Html.Renderer.Text (renderHtml)

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
                    , "*2ccb replied to you:\n\n\
                        \<pre>addition swimming center subject</pre>\n\n\
                        \Read and reply: /issues/1#comment1"
                    , "*2ccb requested you to comment on /issues/1#comment1"
                    ]
  where
    userName = "ff0aca40-ed41-5ab5-8dd4-6dd03ae92ccb"
    forumId = ForumKey "OFFTOPIC"
    issueId = IssueKey 1

    create = do
        userEntity <- createUser userName Nothing
        authenticateAs userEntity

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
        request do
            setMethod "GET"
            setUrl $ IssueR issueId
            addRequestHeader ("Accept", "text/plain")
        printBody
        statusIs 200

        request do
            setMethod "POST"
            setUrl CommentsR
            addToken
            addPostParam "issue" $ toPathPiece issueId
            addPostParam "message" "addition swimming center subject"
            addPostParam "request_user" "1"
        statusIs 303

    getMessages = do
        events <- runDB Event.dbGetUndelivered
        app <- getTestYesod
        let makeMessage' :: SomeEvent -> YesodExample App TextL
            makeMessage' (SomeEvent e) =
                renderHtml <$> runDB (Event.makeMessage app e)
        traverse makeMessage' events
