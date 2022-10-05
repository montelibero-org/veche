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
    , getIssueNewR
    , getIssueR
    , getIssuesR
    , postIssueR
    , postIssuesR
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
import Templates.Issue (closeReopenButton, editIssueForm, issueTable,
                        newIssueForm, voteButtons)
import Templates.User (userNameWidget)

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    IssueMaterialized
            { comments
            , body
            , isCloseReopenAllowed
            , isCommentAllowed
            , isEditAllowed
            , issue = Issue{title, open}
            , isVoteAllowed
            , requests
            , votes
            } <-
        Issue.load issueId

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

getIssueNewR :: Handler Html
getIssueNewR = do
    Entity _ User{stellarAddress} <- requireAuth
    Entity signerId _ <- StellarSigner.getByAddress403 mtlFund stellarAddress
    requireAuthz $ CreateIssue signerId
    formWidget <- generateFormPostB newIssueForm
    defaultLayout formWidget

getIssuesR :: Handler Html
getIssuesR = do
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    issues <- Issue.selectByOpen stateOpen
    (openIssueCount, closedIssueCount) <- Issue.countOpenAndClosed
    defaultLayout $(widgetFile "issues")

postIssuesR :: Handler Void
postIssuesR = do
    (result, formWidget) <- runFormPostB newIssueForm
    case result of
        FormSuccess content -> do
            issueId <- Issue.create content
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
    (result, formWidget) <- runFormPostB $ editIssueForm issueId Nothing
    case result of
        FormSuccess content -> do
            Issue.edit issueId content
            redirect $ IssueR issueId
        _ -> defaultLayout formWidget

getIssueEditR :: IssueId -> Handler Html
getIssueEditR issueId = do
    content <- Issue.getContentForEdit issueId
    formWidget <- generateFormPostB $ editIssueForm issueId $ Just content
    defaultLayout formWidget
