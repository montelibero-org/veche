{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
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
    Key (IssueKey),
    StateAction (..),
    -- * Create
    create,
    -- * Retrieve
    countOpenAndClosed,
    getContentForEdit,
    load,
    listForumIssues,
    selectWithoutVoteFromUser,
    -- * Update
    closeReopen,
    edit,
    -- * Tools
    dbUpdateAllIssueApprovals,
) where

import Import.NoFoundation hiding (groupBy, isNothing)

-- global
import Data.Coerce (coerce)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Database.Esqueleto.Experimental (Value (Value), countRows, from, groupBy,
                                        innerJoin, isNothing, just, leftJoin,
                                        not_, on, select, table, val, where_,
                                        (&&.), (:&) ((:&)), (==.), (?.), (^.))
import Database.Persist (PersistException (PersistForeignConstraintUnmet), get,
                         getBy, getEntity, getJust, insert, insert_, selectList,
                         update, (!=.), (=.))
import Database.Persist qualified as Persist
import Database.Persist.Sql (SqlBackend)
import Yesod.Persist (YesodPersist, YesodPersistBackend, get404, runDB)

-- component
import Genesis (forums, mtlAsset, mtlFund)
import Model (Comment (Comment), Issue (Issue), IssueId,
              IssueVersion (IssueVersion), Key (CommentKey), Request (Request),
              Unique (UniqueHolder, UniqueSigner), User (User), UserId,
              Vote (Vote))
import Model qualified
import Model.Comment (CommentMaterialized (CommentMaterialized))
import Model.Comment qualified
import Model.Forum qualified as Forum
import Model.Request (IssueRequestMaterialized)
import Model.Request qualified as Request
import Model.Vote qualified as Vote

data IssueContent = IssueContent{title, body :: Text, poll :: Maybe Poll}
    deriving (Show)

data VoteMaterialized = VoteMaterialized
    { choice :: Choice
    , voter  :: Entity User
    }

data IssueMaterialized = IssueMaterialized
    { comments              :: Forest CommentMaterialized
    , body                  :: Text
    , forum                 :: Forum
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
    ForumId -> HandlerFor app (Int, Int)
countOpenAndClosed forum = do
    counts :: [(Bool, Int)] <-
        runDB $
            select do
                issue <- from $ table @Issue
                where_ $ issue ^. #forum ==. val forum
                groupBy $ issue ^. #open
                pure (issue ^. #open, countRows @Int)
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
                `on` \(comment :& user) -> comment ^. #author ==. user ^. #id
            where_ $ comment ^. #issue ==. val issueId
            pure (comment, user)

    loadRawRequests ::
        MonadIO m => SqlPersistT m [(Entity Request, Entity User)]
    loadRawRequests =
        select do
            request :& user <- from $
                table @Request `innerJoin` table @User
                `on` \(request :& user) -> request ^. #user ==. user ^. #id
            where_ $ request ^. #issue ==. val issueId
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
                `on` \(vote :& user) -> vote ^. #user ==. user ^. #id
            where_ $ vote ^. #issue ==. val issueId
            pure (vote, user)
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
        issue <- get404 issueId
        let Issue   { author    = authorId
                    , created
                    , curVersion
                    , forum     = forumId
                    } =
                issue
        forumE@(_, forum) <- Forum.getJustEntity forumId

        Entity userId User{stellarAddress} <- requireAuth
        mSigner <- getBy $ UniqueSigner mtlFund  stellarAddress
        mHolder <- getBy $ UniqueHolder mtlAsset stellarAddress
        let mSignerId = entityKey <$> mSigner
            mHolderId = entityKey <$> mHolder
        requireAuthz $ ReadForumIssue forumE (mSignerId, mHolderId)

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
            isCommentAllowed     =
                isAllowed $ AddForumIssueComment forumE (mSignerId, mHolderId)
            isVoteAllowed        = isAllowed $ AddIssueVote issueE mSignerId
        pure
            IssueMaterialized
            { body
            , comments
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

collectChoices :: [VoteMaterialized] -> Map Choice (EntitySet User)
collectChoices votes =
    Map.fromListWith
        (<>)
        [ (choice, Map.singleton uid user)
        | VoteMaterialized{choice, voter = Entity uid user} <- votes
        ]

listForumIssues ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    EntityForum -> Maybe Bool -> HandlerFor app [Entity Issue]
listForumIssues forumE@(forumId, _) mIsOpen =
    runDB do
        Entity _ User{stellarAddress} <- requireAuth
        mSigner <- getBy $ UniqueSigner mtlFund  stellarAddress
        mHolder <- getBy $ UniqueHolder mtlAsset stellarAddress
        let mSignerId = entityKey <$> mSigner
            mHolderId = entityKey <$> mHolder
        requireAuthz $ ListForumIssues forumE (mSignerId, mHolderId)
        selectList filters []
  where
    filters =
        (#forum Persist.==. forumId)
        : [#open Persist.==. isOpen | Just isOpen <- [mIsOpen]]

selectWithoutVoteFromUser ::
    (YesodAuthPersist app, YesodPersistBackend app ~ SqlBackend) =>
    Entity User -> HandlerFor app [Entity Issue]
selectWithoutVoteFromUser (Entity userId User{stellarAddress}) =
    runDB do
        issues <-
            -- TODO(2022-10-08, cblp) filter accessible issues in the DB
            select do
                issue :& vote <- from $
                    table @Issue `leftJoin` table @Vote
                    `on` \(issue :& vote) ->
                        just (issue ^. #id) ==. vote ?. #issue
                        &&. vote ?. #user ==. val (Just userId)
                where_ $
                    (issue ^. #open)
                    &&. not_ (isNothing $ issue ^. #poll)
                    &&. isNothing (vote ?. #id)
                pure issue
        mSigner <- getBy $ UniqueSigner mtlFund  stellarAddress
        mHolder <- getBy $ UniqueHolder mtlAsset stellarAddress
        let mSignerId = entityKey <$> mSigner
            mHolderId = entityKey <$> mHolder
        pure $
            issues
            & filter \(Entity _ Issue{forum}) ->
                isAllowed $
                ListForumIssues (forum, forums ! forum) (mSignerId, mHolderId)

getContentForEdit ::
    ( AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    IssueId -> HandlerFor app (EntityForum, IssueContent)
getContentForEdit issueId =
    runDB do
        userId <- requireAuthId
        issue@(Entity _ Issue{curVersion, forum = forumId, poll, title}) <-
            getEntity404 issueId
        forum <- Forum.getJust forumId
        requireAuthz $ EditIssue issue userId
        versionId <-
            curVersion ?| constraintFail "Issue.current_version must be valid"
        IssueVersion{body} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        pure ((forumId, forum), IssueContent{title, body, poll})

create ::
    ( AuthEntity app ~ User
    , AuthId app ~ UserId
    , YesodAuthPersist app
    , YesodPersistBackend app ~ SqlBackend
    ) =>
    EntityForum -> IssueContent -> HandlerFor app IssueId
create forumE@(forumId, _) content = do
    Entity userId User{stellarAddress} <- requireAuth
    runDB do
        mSigner <- getBy $ UniqueSigner mtlFund  stellarAddress
        mHolder <- getBy $ UniqueHolder mtlAsset stellarAddress
        let mSignerId = entityKey <$> mSigner
            mHolderId = entityKey <$> mHolder
        requireAuthz $ AddForumIssue forumE (mSignerId, mHolderId)
        dbCreate forumId content userId

dbCreate ::
    (HasCallStack, MonadUnliftIO m) =>
    ForumId -> IssueContent -> UserId -> SqlPersistT m IssueId
dbCreate forumId IssueContent{body, poll, title} userId = do
    now <- liftIO getCurrentTime
    let issue =
            Issue
                { approval          = 0
                , author            = userId
                , commentNum        = 0
                , created           = now
                , curVersion        = Nothing
                , eventDelivered    = False
                , forum             = forumId
                , open              = True
                , poll
                , title
                }
    issueId <- addCallStack $ insert issue
    let version =
            IssueVersion{issue = issueId, body, created = now, author = userId}
    versionId <- addCallStack $ insert version
    addCallStack $ update issueId [#curVersion =. Just versionId]
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
        version <-
            maybe
                (throwIO $ PersistForeignConstraintUnmet "issue.version")
                pure
                curVersion
        requireAuthz $ EditIssue (Entity issueId old) user
        IssueVersion{body = oldBody} <- getJust version
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

    updatePoll = update issueId [#poll =. poll]

    updateTitle = update issueId [#title =. title]

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
    for_ issues \(Entity issueId issue@Issue{poll}) ->
        when (isJust poll) $ Vote.dbUpdateIssueApproval issueId $ Just issue
