{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Issue (
    -- * Types
    Issue (..),
    IssueContent (..),
    IssueId,
    IssueMaterialized (..),
    StateAction (..),
    -- * Create
    create,
    -- * Retrieve
    countOpenAndClosed,
    dbSelectAll,
    getContentForEdit,
    load,
    selectAll,
    selectByOpen,
    selectWithoutVoteFromUser,
    -- * Update
    closeReopen,
    edit,
) where

import Import.NoFoundation

-- global
import Data.Map.Strict qualified as Map
import Database.Persist (Filter, Key, get, getBy, getEntity, insert, insert_,
                         selectList, toPersistValue, update, (=.), (==.))
import Database.Persist.Sql (Single, SqlBackend, rawSql, unSingle)
import Yesod.Core (notFound)
import Yesod.Persist (YesodPersist, YesodPersistBackend, get404, runDB)

-- component
import Genesis (mtlAsset, mtlFund)
import Model (Comment (Comment),
              EntityField (Issue_curVersion, Issue_open, Issue_poll, Issue_title),
              Issue (Issue), IssueId, IssueVersion (IssueVersion),
              Key (CommentKey), Request (Request),
              Unique (UniqueHolder, UniqueSigner), User (User), UserId,
              Vote (Vote))
import Model qualified
import Model.Comment (CommentMaterialized (CommentMaterialized))
import Model.Comment qualified
import Model.Request (IssueRequestMaterialized)
import Model.Request qualified as Request

data IssueContent = IssueContent{title, body :: Text, poll :: Maybe Poll}
    deriving (Show)

data VoteMaterialized = VoteMaterialized
    { choice :: Choice
    , voter  :: Entity User
    }

type EntitySet a = Map (Key a) a

data IssueMaterialized = IssueMaterialized
    { comments              :: Forest CommentMaterialized
    , body                  :: Text
    , isCloseReopenAllowed  :: Bool
    , isCommentAllowed      :: Bool
    , isEditAllowed         :: Bool
    , issue                 :: Issue
    , isVoteAllowed         :: Bool
    , requests              :: [IssueRequestMaterialized]
    , votes                 :: Map Choice (EntitySet User)
    }

data StateAction = Close | Reopen
    deriving (Eq)

countOpenAndClosed ::
    (YesodPersist app, YesodPersistBackend app ~ SqlBackend) =>
    HandlerFor app (Int, Int)
countOpenAndClosed = do
    counts :: [(Bool, Int)] <-
        runDB $
            rawSql
                @(Single Bool, Single Int)
                "SELECT open, COUNT(*) FROM issue GROUP BY open"
                []
            <&> map (bimap unSingle unSingle)
    pure
        ( findWithDefault 0 True  counts
        , findWithDefault 0 False counts
        )

loadComments ::
    MonadIO m => IssueId -> SqlPersistT m (Forest CommentMaterialized)
loadComments issueId =
    materializeComments <$> loadRawComments <*> loadRawRequests
  where

    loadRawComments ::
        MonadIO m => SqlPersistT m [(Entity Comment, Entity User)]
    loadRawComments =
        rawSql
            "SELECT ??, ??\
            \ FROM comment, user ON comment.author = user.id\
            \ WHERE comment.issue = ?"
            [toPersistValue issueId]

    loadRawRequests ::
        MonadIO m => SqlPersistT m [(Entity Request, Entity User)]
    loadRawRequests =
        rawSql
            "SELECT ??, ??\
            \ FROM request, user ON request.user = user.id\
            \ WHERE request.issue = ?"
            [toPersistValue issueId]

materializeComments ::
    [(Entity Comment, Entity User)] ->
    [(Entity Request, Entity User)] ->
    Forest CommentMaterialized
materializeComments comments requests =
    unfoldForest materialize topLevelComments
  where

    commentsByParent =
        Map.fromListWith
            (++)
            [ (parent, [commentAndAuthor])
            | commentAndAuthor@(Entity _ Comment{parent}, _) <- comments
            ]
        <&> sortOn (fst >>> entityKey)
            -- assume keys are monotonically increasing

    topLevelComments = findWithDefault [] Nothing commentsByParent

    requestsByComment =
        Map.fromListWith
            (++)
            [ (comment, [user])
            | (Entity _ Request{comment}, Entity _ user) <- requests
            ]

    materialize (Entity id comment, author) =
        ( CommentMaterialized{id, comment, author, requestedUsers}
        , findWithDefault [] (Just id) commentsByParent
        )
      where
        requestedUsers = findWithDefault [] id requestsByComment

loadVotes :: MonadIO m => IssueId -> SqlPersistT m [VoteMaterialized]
loadVotes issueId = do
    votes <-
        rawSql
            @(Entity Vote, Entity User)
            "SELECT ??, ??\
            \ FROM vote, user ON vote.user = user.id\
            \ WHERE vote.issue = ?"
            [toPersistValue issueId]
    pure
        [ VoteMaterialized{choice, voter}
        | (Entity _ Vote{choice}, voter) <- votes
        ]

load ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    IssueId -> HandlerFor app IssueMaterialized
load issueId =
    runDB do
        Entity userId User{stellarAddress} <- requireAuth
        Entity holderId _ <- getBy403 $ UniqueHolder mtlAsset stellarAddress
        requireAuthz $ ReadIssue holderId
        mSignerId <-
            fmap entityKey <$> getBy (UniqueSigner mtlFund stellarAddress)

        issue@Issue{author = authorId, created, curVersion} <- get404 issueId
        versionId <-
            curVersion ?| constraintFail "Issue.current_version must be valid"
        author <-
            getEntity authorId
            ?|> constraintFail "Issue.author must exist in User table"
        comments' <- loadComments issueId
        let comments = startingPseudoComment authorId author created : comments'
        IssueVersion{body} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        votes <- collectChoices <$> loadVotes issueId
        requests <- Request.selectActiveByIssueAndUser issueId userId

        let issueE = Entity issueId issue
            isEditAllowed        = isAllowed $ EditIssue        issueE userId
            isCloseReopenAllowed = isAllowed $ CloseReopenIssue issueE userId
            isCommentAllowed     = isAllowed $ AddIssueComment holderId
            isVoteAllowed        = any (isAllowed . AddVote) mSignerId
        pure
            IssueMaterialized
            { body
            , comments
            , isCloseReopenAllowed
            , isCommentAllowed
            , isEditAllowed
            , issue
            , isVoteAllowed
            , requests
            , votes
            }
  where
    startingPseudoComment authorId author created =
        Node
            CommentMaterialized
                { id = CommentKey 0
                , comment =
                    Comment
                        { author            = authorId
                        , created
                        , eventDelivered    = False
                        , message           = ""
                        , parent            = Nothing
                        , issue             = issueId
                        , type_             = CommentStart
                        }
                , author
                , requestedUsers = []
                }
            []

collectChoices :: [VoteMaterialized] -> Map Choice (EntitySet User)
collectChoices votes =
    Map.fromListWith
        (<>)
        [ (choice, Map.singleton uid user)
        | VoteMaterialized{choice, voter = Entity uid user} <- votes
        ]

selectWith ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    [Filter Issue] -> HandlerFor app [Entity Issue]
selectWith filters =
    runDB do
        Entity _ User{stellarAddress} <- requireAuth
        Entity holderId _ <- getBy403 $ UniqueHolder mtlAsset stellarAddress
        requireAuthz $ ListIssues holderId
        selectList filters []

selectByOpen ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    Bool -> HandlerFor app [Entity Issue]
selectByOpen isOpen = selectWith [Issue_open ==. isOpen]

selectAll ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    HandlerFor app [Entity Issue]
selectAll = selectWith []

dbSelectAll :: MonadIO m => SqlPersistT m [Entity Issue]
dbSelectAll = selectList [] []

selectWithoutVoteFromUser ::
    (YesodAuthPersist app, YesodPersistBackend app ~ SqlBackend) =>
    Entity User -> HandlerFor app [Entity Issue]
selectWithoutVoteFromUser (Entity userId User{stellarAddress}) =
    runDB do
        Entity holderId _ <- getBy403 $ UniqueHolder mtlAsset stellarAddress
        requireAuthz $ ListIssues holderId
        rawSql
            @(Entity Issue)
            "SELECT ??\
            \ FROM\
                \ issue\
                \ LEFT JOIN\
                \ (SELECT * FROM vote WHERE vote.user = ?) AS vote\
                \ ON issue.id = vote.issue\
            \ WHERE vote.id IS NULL AND issue.open"
            [toPersistValue userId]

getContentForEdit ::
    ( AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    IssueId -> HandlerFor app IssueContent
getContentForEdit issueId =
    runDB do
        userId <- requireAuthId
        issue@(Entity _ Issue{title, curVersion, poll}) <-
            getEntity404 issueId
        requireAuthz $ EditIssue issue userId
        versionId <-
            curVersion ?| constraintFail "Issue.current_version must be valid"
        IssueVersion{body} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        pure IssueContent{title, body, poll}

create ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    IssueContent -> HandlerFor app IssueId
create IssueContent{title, body, poll} = do
    now <- liftIO getCurrentTime
    Entity userId User{stellarAddress} <- requireAuth
    runDB do
        Entity signerId _ <- getBy403 $ UniqueSigner mtlFund stellarAddress
        requireAuthz $ CreateIssue signerId
        let issue =
                Issue
                    { approval          = 0
                    , author            = userId
                    , commentNum        = 0
                    , created           = now
                    , curVersion        = Nothing
                    , eventDelivered    = False
                    , open              = True
                    , poll
                    , title
                    }
        issueId <- insert issue
        let version =
                IssueVersion
                    {issue = issueId, body, created = now, author = userId}
        versionId <- insert version
        update issueId [Issue_curVersion =. Just versionId]
        pure issueId

edit ::
    ( AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    IssueId -> IssueContent -> HandlerFor app ()
edit issueId IssueContent{title, body, poll} = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        old@Issue{title = oldTitle, poll = oldPoll, curVersion} <-
            get404 issueId
        version <- maybe notFound pure curVersion
        requireAuthz $ EditIssue (Entity issueId old) user
        IssueVersion{body = oldBody} <- get404 version
        unless (title == oldTitle) updateTitle
        unless (body == oldBody) $ addVersion now user
        unless (poll == oldPoll) updatePoll
        when (title /= oldTitle || body /= oldBody) $ addVersionComment now user
  where

    addVersion now user = do
        let version =
                IssueVersion
                    {author = user, body, created = now, issue = issueId}
        versionId <- insert version
        update issueId [Issue_curVersion =. Just versionId]

    addVersionComment now user =
        insert_
            Comment
                { author            = user
                , created           = now
                , eventDelivered    = False
                , issue             = issueId
                , message           = ""
                , parent            = Nothing
                , type_             = CommentEdit
                }

    updatePoll = update issueId [Issue_poll =. poll]

    updateTitle = update issueId [Issue_title =. title]

closeReopen ::
    ( AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    IssueId -> StateAction -> HandlerFor app ()
closeReopen issueId stateAction = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        issue <- getEntity404 issueId
        requireAuthz $ CloseReopenIssue issue user
        update issueId [Issue_open =. newState]
        insert_
            Comment
                { author            = user
                , created           = now
                , eventDelivered    = False
                , issue             = issueId
                , message           = ""
                , parent            = Nothing
                , type_             = commentType
                }
  where
    (newState, commentType) =
        case stateAction of
            Close  -> (False, CommentClose )
            Reopen -> (True , CommentReopen)
