{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Import

import Database.Persist.Sql (rawSql)
import Text.Julius (rawJS)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

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

getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
    IssueMaterialized{comments, issue, lastEdit} <- runDB $ loadIssue issueId
    let Issue{issueTitle, issueOpen} = issue
    let IssueVersion{issueVersionBody} = lastEdit
    commentFormId <- newIdent
    commentListId <- newIdent
    commentTextareaId <- newIdent
    defaultLayout $(widgetFile "issue")

data IssueContent = IssueContent{title, body :: Text}

issueForm :: Maybe IssueContent -> AForm Handler IssueContent
issueForm previousContent = do
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

getIssueNewR :: Handler Html
getIssueNewR = do
    (formWidget, formEnctype) <-
        generateFormPost $
        renderBootstrap3 BootstrapBasicForm $ issueForm Nothing
    defaultLayout $(widgetFile "issue-new")

getIssuesR :: Handler Html
getIssuesR = do
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    (openIssueCount, closedIssueCount, issues) <-
        runDB $
            (,,)
            <$> count [IssueOpen ==. True]
            <*> count [IssueOpen ==. False]
            <*> selectList [IssueOpen ==. stateOpen] []
    defaultLayout $(widgetFile "issues")

postIssuesR :: Handler Html
postIssuesR = do
    ((result, formWidget), formEnctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm $ issueForm Nothing
    case result of
        FormSuccess issue -> do
            issueId <- addIssue issue
            redirect $ IssueR issueId
        _ -> defaultLayout $(widgetFile "issue-new")

  where

    addIssue :: IssueContent -> Handler IssueId
    addIssue IssueContent{title, body} = do
        now <- liftIO getCurrentTime
        user <- requireAuthId
        runDB do
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
    mAction <- lookupPostParam "action"
    case mAction of
        Just "close"  -> closeReopenIssue Close  issueId
        Just "reopen" -> closeReopenIssue Reopen issueId
        Just "edit"   -> editIssue               issueId
        _ -> invalidArgs ["action must be one of: close, reopen, edit"]

editIssue :: IssueId -> Handler Html
editIssue issueId = do
    ((result, formWidget), formEnctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm $ issueForm Nothing
    case result of
        FormSuccess content -> do
            addIssueVersion content
            redirect $ IssueR issueId
        _ -> defaultLayout $(widgetFile "issue-edit")

  where

    addIssueVersion :: IssueContent -> Handler ()
    addIssueVersion IssueContent{title, body} = do
        now <- liftIO getCurrentTime
        user <- requireAuthId
        runDB do
            issue <- get404 issueId
            authorizeIssueModification issue user
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

closeReopenIssue :: StateAction -> IssueId -> Handler a
closeReopenIssue action issueId = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        issue <- get404 issueId
        authorizeIssueModification issue user
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

authorizeIssueModification :: MonadHandler m => Issue -> UserId -> m ()
authorizeIssueModification Issue{issueAuthor} user =
    when (issueAuthor /= user) $ permissionDenied "Not authorized"

getIssueEditR :: IssueId -> Handler Html
getIssueEditR issueId = do
    content <-
        runDB do
            Issue{issueTitle, issueCurVersion} <- get404 issueId
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
    (formWidget, formEnctype) <-
        generateFormPost $
        renderBootstrap3 BootstrapBasicForm $ issueForm $ Just content
    defaultLayout $(widgetFile "issue-edit")
