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
    ) where

import Import hiding (share)

-- global
import Data.Map.Strict qualified as Map

-- component
import Model.Issue (IssueMaterialized (..), StateAction (Close, Reopen))
import Model.Issue qualified as Issue
import Model.StellarSigner qualified as StellarSigner
import Model.Vote qualified as Vote
import Templates.Comment (commentForm, commentWidget)
import Templates.Issue (actionForm, closeReopenForm, editIssueForm, issueTable,
                        newIssueForm, voteForm)

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    IssueMaterialized
        { comments
        , body
        , isCloseReopenAllowed
        , isEditAllowed
        , issue = Issue{issueTitle, issueOpen}
        , isVoteAllowed
        , requests
        , votes
        } <-
            Issue.load issueId

    signers <- StellarSigner.selectList
    let weights =
            Map.fromList
                [ (stellarSignerKey, stellarSignerWeight)
                | Entity _ signer <- signers
                , let StellarSigner{stellarSignerKey, stellarSignerWeight} =
                        signer
                ]
        voteResults =
            [ (choice, percentage, share)
            | (choice, users) <- Map.assocs votes
            , let
                choiceWeight =
                    sum
                        [ Map.findWithDefault 0 key weights
                        | User{userStellarAddress = key} <- toList users
                        ]
                percentage =
                    fromIntegral choiceWeight / fromIntegral (sum weights) * 100
                    :: Double
                share = show choiceWeight <> "/" <> show (sum weights)
            ]

    closeReopenWidget <- generateFormPostB $ closeReopenForm issueId issueOpen
    voteWidget        <- generateFormPostB $ voteForm        issueId
    (commentFormFields, commentFormEnctype) <-
        generateFormPost $ commentForm (Just issueId) requests

    defaultLayout $(widgetFile "issue")

getIssueNewR :: Handler Html
getIssueNewR = do
    Entity _ User{userStellarAddress} <- requireAuth
    Entity signerId _ <- StellarSigner.getByAddress403 userStellarAddress
    requireAuthz $ CreateIssue signerId
    formWidget <- generateFormPostB newIssueForm
    defaultLayout formWidget

getIssuesR :: Handler Html
getIssuesR = do
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    issues <- Issue.selectList [IssueOpen ==. stateOpen]
    (openIssueCount, closedIssueCount) <- Issue.countOpenAndClosed
    defaultLayout $(widgetFile "issues")

postIssuesR :: Handler Html
postIssuesR = do
    (result, formWidget) <- runFormPostB newIssueForm
    case result of
        FormSuccess content -> do
            issueId <- Issue.create content
            redirect $ IssueR issueId
        _ -> defaultLayout formWidget

postIssueR :: IssueId -> Handler Html
postIssueR issueId = do
    (result, _widget) <- runFormPostB actionForm
    case result of
        FormSuccess action -> doAction action
        _ -> invalidArgs [tshow result]
  where

    doAction = \case
        "approve" -> Vote.record       issueId Approve >> refresh
        "reject"  -> Vote.record       issueId Reject  >> refresh
        "close"   -> Issue.closeReopen issueId Close   >> refresh
        "reopen"  -> Issue.closeReopen issueId Reopen  >> refresh
        "edit"    -> edit issueId
        _         -> invalidArgs [invalidAction]

    refresh = redirect $ IssueR issueId

    invalidAction =
        "action must be one of: approve, reject, close, reopen, edit"

edit :: IssueId -> Handler Html
edit issueId = do
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
