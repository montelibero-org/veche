{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    Key (IssueKey, IssueVersionKey),
    StateAction (..),
    -- * Create
    create,
    dbCreate,
    -- * Retrieve
    countOpenAndClosed,
    getContentForEdit,
    getIssue,
    load,
    listForumIssues,
    selectWithoutVoteFromUser,
    -- * Update
    closeReopen,
    edit,
    -- * Tools
    dbUpdateAllIssueApprovals,
) where

-- prelude
import Authorization
import Foundation.Base
import Import.NoFoundation hiding (groupBy, isNothing)

-- global
import Control.Monad.Fail (fail)
import Data.Coerce (coerce)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Database.Esqueleto.Experimental (Value (Value), countRows, from, groupBy,
                                        innerJoin, isNothing, just, leftJoin,
                                        not_, on, select, table, val, where_,
                                        (&&.), (:&) ((:&)), (==.))
import Database.Persist (PersistException (PersistForeignConstraintUnmet), get,
                         getEntity, getJust, insert, insert_, selectList,
                         update, (!=.), (=.))
import Database.Persist qualified as Persist
import Database.Persist.Sql (runSqlPool)
import Servant qualified
import Yesod.Persist (get404, runDB)

-- project
import Stellar.Simple (Asset)
import Stellar.Simple qualified as Stellar

-- component
import Genesis (forums)
import Model (Comment (Comment), Issue (..), IssueId,
              IssueVersion (IssueVersion), Key (CommentKey), Request (Request),
              User, UserId, Vote (Vote))
import Model qualified
import Model.Comment (CommentMaterialized (CommentMaterialized))
import Model.Comment qualified
import Model.Escrow qualified as Escrow
import Model.Forum qualified as Forum
import Model.Request (IssueRequestMaterialized)
import Model.Request qualified as Request
import Model.User (maybeAuthzRolesDB, maybeAuthzRolesY, requireAuthzRoles)
import Model.Vote qualified as Vote

data IssueContent = IssueContent
    { body          :: Text
    , contacts      :: Maybe Text
    , poll          :: Maybe Poll
    , priceOffer    :: Maybe Text
    , title         :: Text
    }
    deriving (Show)

data VoteMaterialized = VoteMaterialized
    { choice :: Choice
    , voter  :: Entity User
    }

data IssueMaterialized = IssueMaterialized
    { body                  :: Text
    , comments              :: Forest CommentMaterialized
    , escrow                :: Map Asset Scientific
    , forum                 :: Forum
    , isCloseReopenAllowed  :: Bool
    , isCommentAllowed      :: Bool
    , isEditAllowed         :: Bool
    , issue                 :: Issue
    , isVoteAllowed         :: Bool
    , requests              :: [IssueRequestMaterialized]
    , votes                 :: Map Choice (EntitySet User)
    }
    deriving (Show)

data IssueDto = IssueDto
    { issue     :: Issue
    , version   :: IssueVersion
    }
    deriving (Generic, ToJSON)

data StateAction = Close | Reopen
    deriving (Eq)

countOpenAndClosed :: ForumId -> Handler (Int, Int)
countOpenAndClosed forum = do
    counts :: [(Bool, Int)] <-
        runDB $
            select do
                issue <- from $ table @Issue
                where_ $ issue.forum ==. val forum
                groupBy issue.open
                pure (issue.open, countRows @Int)
            <&> coerce
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
        select do
            comment :& user <- from $
                table @Comment `innerJoin` table @User
                `on` \(comment :& user) -> comment.author ==. user.id
            where_ $ comment.issue ==. val issueId
            pure (comment, user)

    loadRawRequests ::
        MonadIO m => SqlPersistT m [(Entity Request, Entity User)]
    loadRawRequests =
        select do
            request :& user <- from $
                table @Request `innerJoin` table @User
                `on` \(request :& user) -> request.user ==. user.id
            where_ $ request.issue ==. val issueId
            pure (request, user)

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
        select do
            vote :& user <- from $
                table @Vote `innerJoin` table @User
                `on` \(vote :& user) -> vote.user ==. user.id
            where_ $ vote.issue ==. val issueId
            pure (vote, user)
    pure
        [ VoteMaterialized{choice, voter}
        | (Entity _ Vote{choice}, voter) <- votes
        ]

load :: IssueId -> Handler IssueMaterialized
load issueId = do
    (mUserId, roles) <- maybeAuthzRolesY
    let authenticated = isJust mUserId
    app <- getYesod
    escrow <- Escrow.getIssueBalance issueId app
    runDB do
        issue <- get404 issueId
        let Issue{author = authorId, created, curVersion, forum = forumId} =
                issue
        forumE@(_, forum) <- Forum.getJustEntity forumId

        requireAuthz $ ReadForumIssue forumE roles

        versionId <-
            curVersion ?| constraintFail "Issue.current_version must be valid"
        author <-
            getEntity authorId
            ?|> constraintFail "Issue.author must exist in User table"
        comments <-
            if authenticated then
                (startingPseudoComment authorId author created :)
                <$> loadComments issueId
            else
                pure []
        IssueVersion{body} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        votes <- collectChoices <$> loadVotes issueId
        requests <-
            case mUserId of
                Just userId ->
                    Request.selectActiveByIssueAndRequestedUser issueId userId
                Nothing     -> pure []

        let issueE = Entity issueId issue
            isEditAllowed = any (isAllowed . EditIssue issueE) mUserId
            isCloseReopenAllowed =
                any (isAllowed . CloseReopenIssue issueE) mUserId
            isCommentAllowed =
                authenticated && isAllowed (AddForumIssueComment forumE roles)
            isVoteAllowed =
                authenticated && isAllowed (AddIssueVote issueE roles)
        pure
            IssueMaterialized
                { body
                , comments
                , escrow
                , forum
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

getIssue :: App -> Stellar.Address -> IssueId -> Servant.Handler IssueDto
getIssue app userAddress issueId = do
    -- escrow <- Escrow.getIssueBalance issueId app
    ((_, roles), issue, forumE) <-
        liftIO $ (`runSqlPool` appConnPool) do
            mauth  <- maybeAuthzRolesDB userAddress
            issue  <- get404 issueId
            forumE <- Forum.getJustEntity issue.forum
            pure (mauth, issue, forumE)

    -- let mUserId = entityKey <$> mUserE
    let Issue{curVersion} = issue

    requireAuthzS $ ReadForumIssue forumE roles

    liftIO $ (`runSqlPool` appConnPool) do
        versionId <- curVersion ?| fail "Issue.current_version must be valid"
        -- comments <- loadComments issueId
        version <-
            get versionId
            ?|> fail "Issue.current_version must exist in IssueVersion table"
        -- votes <- collectChoices <$> loadVotes issueId
        -- requests <-
        --     case mUserId of
        --         Nothing     -> pure []
        --         Just userId ->
        --             Request.selectActiveByIssueAndRequestedUser issueId userId

        -- let issueE = Entity issueId issue
        -- let isEditAllowed = any (isAllowed . EditIssue issueE) mUserId
        -- let isCloseReopenAllowed =
        --         any (isAllowed . CloseReopenIssue issueE) mUserId
        -- let isCommentAllowed = isAllowed $ AddForumIssueComment forumE roles
        -- let isVoteAllowed    = isAllowed $ AddIssueVote         issueE roles
        pure IssueDto{issue, version}
  where
    App{appConnPool} = app

collectChoices :: [VoteMaterialized] -> Map Choice (EntitySet User)
collectChoices votes =
    Map.fromListWith
        (<>)
        [ (choice, Map.singleton uid user)
        | VoteMaterialized{choice, voter = Entity uid user} <- votes
        ]

listForumIssues :: EntityForum -> Maybe Bool -> Handler [Entity Issue]
listForumIssues forumE@(forumId, _) mIsOpen = do
    (_, roles) <- maybeAuthzRolesY
    requireAuthz $ ReadForum forumE roles
    runDB $ selectList filters []
  where
    filters =
        (#forum Persist.==. forumId)
        : [#open Persist.==. isOpen | Just isOpen <- [mIsOpen]]

selectWithoutVoteFromUser :: Handler [Entity Issue]
selectWithoutVoteFromUser = do
    (userId, roles) <- requireAuthzRoles
    runDB do
        issues <-
            -- TODO(2022-10-08, cblp) filter accessible issues in the DB
            select do
                issue :& vote <- from $
                    table @Issue `leftJoin` table @Vote
                    `on` \(issue :& vote) ->
                        just issue.id ==. vote.issue
                        &&. vote.user ==. val (Just userId)
                where_ $
                    issue.open
                    &&. not_ (isNothing issue.poll)
                    &&. isNothing vote.id
                pure issue
        pure $
            issues
            & filter \(Entity _ Issue{forum}) ->
                isAllowed $ ReadForum (forum, forums ! forum) roles

getContentForEdit :: IssueId -> Handler (EntityForum, IssueContent)
getContentForEdit issueId =
    runDB do
        userId <- requireAuthId
        issueE <- getEntity404 issueId
        let Issue   { contacts
                    , curVersion
                    , forum = forumId
                    , poll
                    , priceOffer
                    , title
                    } =
                entityVal issueE
        forum <- Forum.getJust forumId
        requireAuthz $ EditIssue issueE userId
        versionId <-
            curVersion ?| constraintFail "Issue.current_version must be valid"
        IssueVersion{body} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        pure
            ( (forumId, forum)
            , IssueContent{title, body, poll, contacts, priceOffer}
            )

create :: EntityForum -> IssueContent -> Handler IssueId
create forumE@(forumId, _) content = do
    (userId, roles) <- requireAuthzRoles
    requireAuthz $ AddForumIssue forumE roles
    runDB $ dbCreate forumId content userId

dbCreate ::
    (HasCallStack, MonadUnliftIO m) =>
    ForumId -> IssueContent -> UserId -> SqlPersistT m IssueId
dbCreate forumId issueContent userId = do
    now <- liftIO getCurrentTime
    let issue =
            Issue
                { approval          = 0
                , author            = userId
                , commentNum        = 0
                , contacts
                , created           = now
                , curVersion        = Nothing
                , eventDelivered    = False
                , forum             = forumId
                , open              = True
                , poll
                , priceOffer
                , title
                }
    issueId <- addCallStack $ insert issue
    let version =
            IssueVersion{issue = issueId, body, created = now, author = userId}
    versionId <- addCallStack $ insert version
    addCallStack $ update issueId [#curVersion =. Just versionId]
    pure issueId
  where
    IssueContent{body, contacts, poll, priceOffer, title} = issueContent

edit :: IssueId -> IssueContent -> Handler ()
edit issueId IssueContent{body, contacts, poll, priceOffer, title} = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        old <- get404 issueId
        let Issue   { contacts      = oldContacts
                    , curVersion
                    , poll          = oldPoll
                    , priceOffer    = oldPriceOffer
                    , title         = oldTitle
                    } =
                old
        version <-
            maybe
                (throwIO $ PersistForeignConstraintUnmet "issue.version")
                pure
                curVersion
        requireAuthz $ EditIssue (Entity issueId old) user
        IssueVersion{body = oldBody} <- getJust version
        when
            (   contacts    /= oldContacts
            ||  priceOffer  /= oldPriceOffer
            ||  title       /= oldTitle
            )
            updateIssue
        when (body /= oldBody) $ addVersion now user
        when (poll /= oldPoll) $ updatePoll old
        when (title /= oldTitle || body /= oldBody) $ addVersionComment now user
  where

    addVersion now user = do
        let version =
                IssueVersion
                    {author = user, body, created = now, issue = issueId}
        versionId <- insert version
        update issueId [#curVersion =. Just versionId]

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

    updatePoll old = do
        update issueId [#poll =. poll]
        let new = old{Model.poll = poll}
        Vote.dbUpdateIssueApproval $ Entity issueId new

    updateIssue =
        update
            issueId
            [#contacts =. contacts, #priceOffer =. priceOffer, #title =. title]

closeReopen :: IssueId -> StateAction -> Handler ()
closeReopen issueId stateAction = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        issue <- getEntity404 issueId
        requireAuthz $ CloseReopenIssue issue user
        update issueId [#open =. newState]
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

dbUpdateAllIssueApprovals :: (HasCallStack, MonadUnliftIO m) => SqlPersistT m ()
dbUpdateAllIssueApprovals = do
    issues <- selectList [#open Persist.==. True, #poll !=. Nothing] []
    for_ issues \issueE@(Entity _ Issue{poll}) ->
        when (isJust poll) $ Vote.dbUpdateIssueApproval issueE
