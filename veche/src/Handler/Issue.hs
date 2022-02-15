{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Database.Persist.Sql (rawSql)
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Genesis (mtlFund)
import Handler.Comment (CommentMaterialized (..), commentWidget)
import Types (CommentType (..))

data IssueMaterialized = IssueMaterialized
    { issue     :: Issue
    , comments  :: [CommentMaterialized]
    , lastEdit  :: IssueVersion
    }

loadIssueComments :: IssueId -> SqlPersistT Handler [CommentMaterialized]
loadIssueComments issueId = do
    comments <-
        rawSql
            "SELECT ??, ??\
            \ FROM Comment, User ON Comment.author == User.id\
            \ WHERE Comment.issue == ?"
            [toPersistValue issueId]
    pure
        [ CommentMaterialized{..}
        | (Entity _ comment, Entity _ author) <- comments
        ]

loadIssue :: IssueId -> SqlPersistT Handler IssueMaterialized
loadIssue issueId = do
    issue@Issue{issueAuthor, issueCreated, issueCurVersion} <- get404 issueId
    versionId <-
        issueCurVersion
        ?| lift (constraintFail "Issue.current_version must be valid")
    author <-
        get issueAuthor
        ?|> lift (constraintFail "Issue.author must exist in User table")
    comments' <- loadIssueComments issueId
    let startingPseudoComment =
            CommentMaterialized
                { comment =
                    Comment
                        { commentAuthor     = issueAuthor
                        , commentCreated    = issueCreated
                        , commentMessage    = ""
                        , commentParent     = Nothing
                        , commentIssue      = issueId
                        , commentType       = CommentStart
                        }
                , author
                }
    let comments = startingPseudoComment : comments'
    lastEdit <-
        get versionId
        ?|> lift
                (constraintFail
                    "Issue.current_version must exist in IssueVersion table")
    pure IssueMaterialized{..}

(?|) :: Applicative f => Maybe a -> f a -> f a
Nothing ?| action   = action
Just x  ?| _        = pure x

(?|>) :: Monad f => f (Maybe a) -> f a -> f a
m ?|> k = m >>= (?| k)

actionButton :: Text -> Text -> [Text] -> AForm Handler Void
actionButton value label extraClasses =
    submitButtonReq SubmitButton{name = "action", value, label, extraClasses}
    $> error "Void"

-- | Generate-only form; for its input, one must use 'actionForm'
closeReopenForm :: IssueId -> Bool -> Form Void
closeReopenForm issueId issueOpen =
    (bform
        if issueOpen then
            actionButton "close" "Close" ["btn-danger"]
        else
            actionButton "reopen" "Reopen" ["btn-success"]
    )
        {action = Just $ IssueR issueId}

-- | Generate-only form; for its input, one must use 'actionForm'
voteForm :: IssueId -> Form Void
voteForm issueId =
    (bform
        (  actionButton "approve" "Approve" ["btn-success"]
        *> actionButton "reject"  "Reject"  ["btn-danger"]
        )
    )
        {action = Just $ IssueR issueId, extraClasses = ["form-inline"]}

actionForm :: Form Text
actionForm = bform $ areq textField ""{fsName = Just "action"} Nothing

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    (userId, User{userStellarAddress}) <- requireAuthPair
    Entity signerId _ <-
        runDB $ getBy403 $ UniqueSigner mtlFund userStellarAddress
    requireAuthz $ ReadIssue signerId

    IssueMaterialized{comments, issue, lastEdit} <- runDB $ loadIssue issueId
    let Issue{issueTitle, issueOpen} = issue
        IssueVersion{issueVersionBody} = lastEdit
        issueE = Entity issueId issue
    let isEditAllowed        = isAllowed $ EditIssue        issueE userId
        isCloseReopenAllowed = isAllowed $ CloseReopenIssue issueE userId
        isVoteAllowed        = isAllowed $ AddVote signerId

    signers <- runDB $ selectList [StellarSignerTarget ==. mtlFund] []
    let weights =
            Map.fromList
                [ (stellarSignerKey, stellarSignerWeight)
                | Entity _ signer <- signers
                , let StellarSigner{stellarSignerKey, stellarSignerWeight} =
                        signer
                ]
        votes =
            [ (vote, percentage, share)
            | (vote, users) <- Map.assocs $ collectVotes comments
            , let
                voteWeight =
                    sum
                        [ Map.findWithDefault 0 key weights
                        | User{userStellarAddress = key} <- toList users
                        ]
                percentage =
                    fromIntegral voteWeight / fromIntegral (sum weights) * 100
                    :: Double
                share = show voteWeight <> "/" <> show (sum weights)
            ]

    closeReopenWidget <- generateFormPostB $ closeReopenForm issueId issueOpen
    voteWidget        <- generateFormPostB $ voteForm        issueId

    defaultLayout $(widgetFile "issue")

data IssueContent = IssueContent{title, body :: Text}

issueForm :: Maybe IssueContent -> Form IssueContent
issueForm previousContent =
    bform do
        title <-
            areq
                textField
                (bfs ("Title" :: Text)){fsName = Just "title"}
                (title <$> previousContent)
        body <-
            unTextarea <$>
            areq
                textareaField
                (bfs ("Message" :: Text)){fsName = Just "body"}
                (Textarea . body <$> previousContent)
        pure IssueContent{..}

editIssueForm :: IssueId -> Maybe IssueContent -> Form IssueContent
editIssueForm issueId previousContent =
    (issueForm previousContent)
        { action = Just $ IssueR issueId
        , footer =
            [whamlet|
                <div .pull-left>
                    <a .btn .btn-default href=@{IssueR issueId}>Cancel
                <div .pull-right>
                    <button .btn .btn-success
                            type=submit name=action value=edit>
                        Save
            |]
        }

newIssueForm :: Form IssueContent
newIssueForm =
    (issueForm Nothing)
        { action = Just IssuesR
        , footer =
            [whamlet|<button type=submit .btn .btn-success>Start dicussion|]
        }

getIssueNewR :: Handler Html
getIssueNewR = do
    runDB do
        (_, User{userStellarAddress}) <- requireAuthPair
        Entity signerId _ <- getBy403 $ UniqueSigner mtlFund userStellarAddress
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
                getBy403 $ UniqueSigner mtlFund userStellarAddress
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
                getBy403 $ UniqueSigner mtlFund userStellarAddress
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

data Vote = Approve | Reject
    deriving (Eq, Ord, Show)

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

addVote :: Vote -> IssueId -> Handler Html
addVote vote issueId = do
    now <- liftIO getCurrentTime
    (user, User{userStellarAddress}) <- requireAuthPair
    runDB do
        Entity signerId _ <- getBy403 $ UniqueSigner mtlFund userStellarAddress
        requireAuthz $ AddVote signerId
        insert_
            Comment
                { commentAuthor     = user
                , commentCreated    = now
                , commentMessage    = ""
                , commentParent     = Nothing
                , commentIssue      = issueId
                , commentType       =
                    case vote of
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
    userId <- requireAuthId
    content <-
        runDB do
            issue@(Entity _ Issue{issueTitle, issueCurVersion}) <-
                getEntity404 issueId
            requireAuthz $ EditIssue issue userId
            versionId <-
                issueCurVersion
                ?| lift (constraintFail "Issue.current_version must be valid")
            IssueVersion{issueVersionBody} <-
                get versionId
                ?|> lift
                        (constraintFail
                            "Issue.current_version must exist\
                            \ in IssueVersion table")
            pure IssueContent{title = issueTitle, body = issueVersionBody}
    formWidget <- generateFormPostB $ editIssueForm issueId $ Just content
    defaultLayout formWidget

collectVotes :: [CommentMaterialized] -> Map Vote (HashSet User)
collectVotes comments =
    Map.fromListWith
        (<>)
        [ (vote, HashSet.singleton author)
        | (author, (_, vote)) <- HashMap.toList lastVotes
        ]
  where
    lastVotes :: HashMap User (UTCTime, Vote)
    lastVotes =
        HashMap.fromListWith
            (maxOn fst)
            [ (author, (commentCreated, vote))
            | CommentMaterialized
                {author, comment = Comment{commentType, commentCreated}} <-
                    comments
            , Just vote <-
                pure
                    case commentType of
                        CommentApprove -> Just Approve
                        CommentReject  -> Just Reject
                        _              -> Nothing
            ]
