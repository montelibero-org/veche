{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Genesis (mtlFund)
import Model.Issue (IssueMaterialized (..))
import Model.Issue qualified as Issue
import Templates.Comment (commentWidget)
import Templates.Issue (actionForm, closeReopenForm, editIssueForm,
                        newIssueForm, voteForm)
import Types.Issue (IssueContent (..))

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    IssueMaterialized
        { comments
        , body
        , isCloseReopenAllowed
        , isEditAllowed
        , issue = Issue{issueTitle, issueOpen}
        , isVoteAllowed
        , votes
        } <-
            Issue.load issueId

    signers <- runDB $ selectList [StellarSignerTarget ==. mtlFund] []
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

    defaultLayout $(widgetFile "issue")

getIssueNewR :: Handler Html
getIssueNewR = do
    runDB do
        (_, User{userStellarAddress}) <- requireAuthPair
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ CreateIssue signerId
    formWidget <- generateFormPostB newIssueForm
    defaultLayout formWidget

getIssuesR :: Handler Html
getIssuesR = do
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    (_, User{userStellarAddress}) <- requireAuthPair
    (openIssueCount, closedIssueCount, issues) <-
        runDB do
            Entity signerId _ <-
                getBy403 $ UniqueMember mtlFund userStellarAddress
            requireAuthz $ ListIssues signerId
            (,,)
                <$> count [IssueOpen ==. True]
                <*> count [IssueOpen ==. False]
                <*> selectList [IssueOpen ==. stateOpen] []
    defaultLayout $(widgetFile "issues")

postIssuesR :: Handler Html
postIssuesR = do
    (result, formWidget) <- runFormPostB newIssueForm
    case result of
        FormSuccess issue -> do
            issueId <- addIssue issue
            redirect $ IssueR issueId
        _ -> defaultLayout formWidget

  where

    addIssue :: IssueContent -> Handler IssueId
    addIssue IssueContent{title, body} = do
        now <- liftIO getCurrentTime
        runDB do
            (user, User{userStellarAddress}) <- requireAuthPair
            Entity signerId _ <-
                getBy403 $ UniqueMember mtlFund userStellarAddress
            requireAuthz $ CreateIssue signerId
            let issue = Issue
                    { issueTitle        = title
                    , issueAuthor       = user
                    , issueOpen         = True
                    , issueCreated      = now
                    , issueCurVersion   = Nothing
                    }
            issueId <- insert issue
            let version = IssueVersion
                    { issueVersionIssue     = issueId
                    , issueVersionBody      = body
                    , issueVersionCreated   = now
                    , issueVersionAuthor    = user
                    }
            versionId <- insert version
            update issueId [IssueCurVersion =. Just versionId]
            pure issueId

data StateAction = Close | Reopen

postIssueR :: IssueId -> Handler Html
postIssueR issueId = do
    (result, _widget) <- runFormPostB actionForm
    case result of
        FormSuccess action ->
            case action of
                "approve" -> addVote     Approve issueId
                "reject"  -> addVote     Reject  issueId
                "close"   -> changeState Close   issueId
                "reopen"  -> changeState Reopen  issueId
                "edit"    -> edit                issueId
                _ ->
                    invalidArgs
                        [   "action must be one of: approve, reject, close,\
                            \ reopen, edit"
                        ]
        _ -> invalidArgs [tshow result]

edit :: IssueId -> Handler Html
edit issueId = do
    (result, formWidget) <- runFormPostB $ editIssueForm issueId Nothing
    case result of
        FormSuccess content -> do
            addIssueVersion content
            redirect $ IssueR issueId
        _ -> defaultLayout formWidget

  where

    addIssueVersion :: IssueContent -> Handler ()
    addIssueVersion IssueContent{title, body} = do
        now <- liftIO getCurrentTime
        user <- requireAuthId
        runDB do
            issue <- getEntity404 issueId
            requireAuthz $ EditIssue issue user
            let version = IssueVersion
                    { issueVersionAuthor    = user
                    , issueVersionBody      = body
                    , issueVersionCreated   = now
                    , issueVersionIssue     = issueId
                    }
            versionId <- insert version
            update
                issueId
                [IssueTitle =. title, IssueCurVersion =. Just versionId]
            insert_
                Comment
                    { commentAuthor     = user
                    , commentCreated    = now
                    , commentMessage    = ""
                    , commentParent     = Nothing
                    , commentIssue      = issueId
                    , commentType       = CommentEdit
                    }

addVote :: Choice -> IssueId -> Handler Html
addVote choice issueId = do
    now <- liftIO getCurrentTime
    (user, User{userStellarAddress}) <- requireAuthPair
    runDB do
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ AddVote signerId
        upsert_
            Vote{voteUser = user, voteIssue = issueId, voteChoice = choice}
            [VoteChoice =. choice]
        insert_
            Comment
                { commentAuthor     = user
                , commentCreated    = now
                , commentMessage    = ""
                , commentParent     = Nothing
                , commentIssue      = issueId
                , commentType       =
                    case choice of
                        Approve -> CommentApprove
                        Reject  -> CommentReject
                }
    redirect $ IssueR issueId

changeState :: StateAction -> IssueId -> Handler a
changeState action issueId = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        issue <- getEntity404 issueId
        requireAuthz $ CloseReopenIssue issue user
        update
            issueId
            [ IssueOpen
                =.  case action of
                        Close  -> False
                        Reopen -> True
            ]
        insert_
            Comment
                { commentAuthor     = user
                , commentCreated    = now
                , commentMessage    = ""
                , commentParent     = Nothing
                , commentIssue      = issueId
                , commentType       =
                    case action of
                        Close   -> CommentClose
                        Reopen  -> CommentReopen
                }
    redirect $ IssueR issueId

getIssueEditR :: IssueId -> Handler Html
getIssueEditR issueId = do
    content <- Issue.getContentForEdit issueId
    formWidget <- generateFormPostB $ editIssueForm issueId $ Just content
    defaultLayout formWidget
