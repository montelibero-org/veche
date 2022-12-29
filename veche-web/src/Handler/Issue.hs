{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Stellar.Simple qualified as Stellar
import Text.Printf (printf)

-- component
import Genesis (escrowAddress, escrowFederatedHost, fcmAsset, mtlAsset, mtlFund,
                mtlIssuer, showKnownAsset, vecheAsset)
import Model.Forum qualified as Forum
import Model.Issue (Issue (Issue), IssueId,
                    IssueMaterialized (IssueMaterialized),
                    StateAction (Close, Reopen))
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified as StellarHolder
import Model.StellarSigner (StellarSigner (StellarSigner))
import Model.StellarSigner qualified as StellarSigner
import Model.User (User (User))
import Model.User qualified
import Model.Vote qualified as Vote
import Templates.Comment (commentForestWidget, commentForm)
import Templates.Issue (closeReopenButton, editIssueForm, newIssueForm,
                        voteButtons)
import Templates.Stellar (renderTx)
import Templates.User (userNameWidget)

-- | Stellar federated address for the issue
issueEscrowAddress :: IssueId -> Text
issueEscrowAddress issueId =
    "E" <> toPathPiece issueId <> "*" <> escrowFederatedHost

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    authnUser <- maybeAuthId
    issueMaterialized <- Issue.load issueId
    let IssueMaterialized
                { attachmentTx
                , body
                , comments
                , escrow
                , forum = Forum{enablePriceOffer, title = forumTitle}
                , isCloseReopenAllowed
                , isCommentAllowed
                , isEditAllowed
                , issue =
                    Issue
                        { contacts
                        , forum = forumId
                        , open
                        , poll
                        , priceOffer
                        , title
                        }
                , requests
                } =
            issueMaterialized
    pollWidget <- makePollWidget poll issueId issueMaterialized
    (commentFormFields, commentFormEnctype) <-
        generateFormPost $ commentForm $ Just (issueId, requests)
    defaultLayout $(widgetFile "issue")
  where
    availableToPay = (* 0.98)
    Stellar.Address escrowAddressT = escrowAddress

makePollWidget :: Maybe Poll -> IssueId -> IssueMaterialized -> Handler Widget
makePollWidget mPoll issueId IssueMaterialized{isVoteAllowed, votes} =
    case mPoll of
        Nothing -> pure mempty
        Just poll -> do
            weights <- getWeights poll
            let voteResults = getVoteResults weights
            currentChoice <- getCurrentChoice
            pure $(widgetFile "poll")
  where

    getWeights = \case
        ByAmountOfFcm   -> getWeightsByAmountOfAsset fcmAsset
        ByAmountOfVeche -> getWeightsByAmountOfAsset vecheAsset
        ByMtlAmount     -> getWeightsByAmountOfAsset mtlAsset
        BySignerWeight  -> do
            accounts <- StellarSigner.getAll mtlFund
            pure $
                Map.fromList
                    [ (key, fromIntegral weight :: Double)
                    | Entity _ StellarSigner{key, weight} <- accounts
                    ]

    getWeightsByAmountOfAsset asset = do
        accounts <- StellarHolder.getAll asset
        pure $
            Map.fromList
                [ (key, realToFrac amount :: Double)
                | Entity _ StellarHolder{key, amount} <- accounts
                ]

    getVoteResults weights =
        [ (choice, percentage, share, toList users :: [User])
        | (choice, users) <- Map.assocs votes
        , let
            choiceWeight =
                sum [ Map.findWithDefault 0 key weights
                    | User{stellarAddress = key} <- toList users
                    ]
            percentage = realToFrac (choiceWeight / sum weights * 100) :: Double
            share =
                printf
                    "%d/%d"
                    (round choiceWeight :: Int)
                    (round $ sum weights :: Int)
                :: String
        ]

    getCurrentChoice = do
        mUserId <- maybeAuthId
        pure $
            do  curUser <- mUserId
                listToMaybe
                    [ choice
                    | (choice, users) <- Map.assocs votes
                    , curUser `member` users
                    ]
            & fromMaybe Abstain

showChoice :: Choice -> Text
showChoice = \case
    Approve -> "👍 Approve"
    Reject  -> "👎 Against"
    Abstain -> "◯ Abstain"

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
