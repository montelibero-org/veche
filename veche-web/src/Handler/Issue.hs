{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Issue
    ( getIssueEditR
    , getForumIssueNewR
    , getIssueR
    , postIssueR
    , postForumIssuesR
    , postIssueCloseR
    , postIssueReopenR
    , postIssueVoteR
    ) where

import Import

-- global
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (badRequest400)

-- component
import Genesis (mtlFund)
import Model.Forum (Forum (Forum), ForumId)
import Model.Forum qualified as Forum
import Model.Issue (Issue (Issue), IssueId,
                    IssueMaterialized (IssueMaterialized),
                    StateAction (Close, Reopen))
import Model.Issue qualified as Issue
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified as StellarSigner
import Model.User (User (User))
import Model.User qualified
import Model.Vote qualified as Vote
import Templates.Comment (commentForestWidget, commentForm)
import Templates.Issue (closeReopenButton, editIssueForm, newIssueForm,
                        voteButtons)
import Templates.User (userNameWidget)

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    IssueMaterialized
            { comments
            , body
            , forum = Forum{title = forumTitle}
            , isCloseReopenAllowed
            , isCommentAllowed
            , isEditAllowed
            , issue = Issue{forum = forumId, open, poll, title}
            , isVoteAllowed
            , requests
            , votes
            } <-
        Issue.load issueId

    -- TODO if poll
    signers <- StellarSigner.selectAll mtlFund
    let weights =
            Map.fromList
                [(key, weight) | Entity _ StellarSigner{key, weight} <- signers]
        voteResults =
            [ (choice, percentage, share, toList users)
            | (choice, users) <- Map.assocs votes
            , let
                choiceWeight =
                    sum
                        [ Map.findWithDefault 0 key weights
                        | User{stellarAddress = key} <- toList users
                        ]
                percentage =
                    fromIntegral choiceWeight / fromIntegral (sum weights) * 100
                    :: Double
                share = show choiceWeight <> "/" <> show (sum weights)
            ]

    (commentFormFields, commentFormEnctype) <-
        generateFormPost $ commentForm (Just issueId) requests
    defaultLayout $(widgetFile "issue")

getForumIssueNewR :: ForumId -> Handler Html
getForumIssueNewR forumId = do
    {-  No sense in checking permissions here:
        - link here is already checked;
        - no sensible information exposed;
        - form sumbission is checked. -}
    forumE <- Forum.getEntity404 forumId
    formWidget <- generateFormPostB $ newIssueForm forumE
    defaultLayout formWidget

postForumIssuesR :: ForumId -> Handler Void
postForumIssuesR forumId = do
    forumE <- Forum.getEntity404 forumId
    (result, formWidget) <- runFormPostB $ newIssueForm forumE
    case result of
        FormSuccess content -> do
            issueId <- Issue.create forumE content
            redirect $ IssueR issueId
        _ -> do
            page <- defaultLayout formWidget
            sendResponseStatus badRequest400 page

postIssueVoteR :: IssueId -> Choice -> Handler ()
postIssueVoteR issueId choice = do
    Vote.record issueId choice
    redirect $ IssueR issueId

postIssueCloseR :: IssueId -> Handler ()
postIssueCloseR = closeReopen Close

postIssueReopenR :: IssueId -> Handler ()
postIssueReopenR = closeReopen Reopen

closeReopen :: StateAction -> IssueId -> Handler ()
closeReopen action issueId = do
    Issue.closeReopen issueId action
    redirect $ IssueR issueId

postIssueR :: IssueId -> Handler Html
postIssueR issueId = do
    forumE <- Forum.getEntityByIssue404 issueId
    (result, formWidget) <- runFormPostB $ editIssueForm forumE issueId Nothing
    case result of
        FormSuccess content -> do
            Issue.edit issueId content
            redirect $ IssueR issueId
        _ -> defaultLayout formWidget

getIssueEditR :: IssueId -> Handler Html
getIssueEditR issueId = do
    (forumE, content) <- Issue.getContentForEdit issueId
    formWidget <-
        generateFormPostB $ editIssueForm forumE issueId $ Just content
    defaultLayout formWidget
