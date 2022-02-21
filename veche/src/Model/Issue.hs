{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Issue (
    IssueMaterialized (..),
    StateAction (..),
    -- * Create
    create,
    -- * Retrieve
    countOpenAndClosed,
    getContentForEdit,
    load,
    Model.Issue.selectList,
    selectWithoutVoteFromUser,
    -- * Update
    closeReopen,
    edit,
) where

import Import

-- global
import Data.Coerce (coerce)
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Database.Persist qualified as Persist
import Database.Persist.Sql (Single (..), rawSql)

-- component
import Genesis (mtlFund)
import Types.Comment (CommentMaterialized (..))
import Types.Issue (IssueContent (..))

data VoteMaterialized = VoteMaterialized
    { choice :: Choice
    , voter  :: User
    }

data IssueMaterialized = IssueMaterialized
    { comments              :: [CommentMaterialized]
    , body                  :: Text
    , isCloseReopenAllowed  :: Bool
    , isEditAllowed         :: Bool
    , issue                 :: Issue
    , isVoteAllowed         :: Bool
    , votes                 :: Map Choice (HashSet User)
    }

data StateAction = Close | Reopen

countOpenAndClosed :: Handler (Int, Int)
countOpenAndClosed = do
    counts' <-
        runDB $
        rawSql
            @(Single Bool, Single Int)
            "SELECT open, COUNT(*) FROM issue GROUP BY open"
            []
    let counts = coerce counts' :: [(Bool, Int)]
    pure
        ( findWithDefault 0 True  counts
        , findWithDefault 0 False counts
        )

loadComments :: MonadIO m => IssueId -> SqlPersistT m [CommentMaterialized]
loadComments issueId = do
    comments <-
        rawSql
            @(Entity Comment, Entity User)
            "SELECT ??, ??\
            \ FROM comment, user ON comment.author = user.id\
            \ WHERE comment.issue = ?"
            [toPersistValue issueId]
    requests <-
        rawSql
            @(Entity Request, Entity User)
            "SELECT ??, ??\
            \ FROM request, user ON request.user = user.id\
            \ WHERE request.issue = ?"
            [toPersistValue issueId]
    let requestsByComment =
            Map.fromListWith
                (++)
                [ (requestComment, [user])
                | (Entity _ Request{requestComment}, Entity _ user) <- requests
                ]
    for comments \(Entity id comment, Entity _ author) -> do
        let requestedUsers = findWithDefault [] id requestsByComment
        pure CommentMaterialized{id, comment, author, requestedUsers}

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
        [ VoteMaterialized{choice = voteChoice, voter}
        | (Entity _ Vote{voteChoice}, Entity _ voter) <- votes
        ]

load :: IssueId -> Handler IssueMaterialized
load issueId =
    runDB do
        Entity userId User{userStellarAddress} <- requireAuth
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ ReadIssue signerId

        issue@Issue{issueAuthor = authorId, issueCreated, issueCurVersion} <-
            get404 issueId
        versionId <-
            issueCurVersion
            ?| constraintFail "Issue.current_version must be valid"
        author <-
            get authorId
            ?|> constraintFail "Issue.author must exist in User table"
        comments' <- loadComments issueId
        let comments =
                startingPseudoComment authorId author issueCreated : comments'
        IssueVersion{issueVersionBody = body} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        votes <- collectChoices <$> loadVotes issueId

        let issueE = Entity issueId issue
            isEditAllowed        = isAllowed $ EditIssue        issueE userId
            isCloseReopenAllowed = isAllowed $ CloseReopenIssue issueE userId
            isVoteAllowed        = isAllowed $ AddVote signerId
        pure IssueMaterialized{..}
  where
    startingPseudoComment commentAuthor author commentCreated =
        CommentMaterialized
            { id = fromBackendKey 0
            , comment =
                Comment
                    { commentAuthor
                    , commentCreated
                    , commentMessage = ""
                    , commentParent  = Nothing
                    , commentIssue   = issueId
                    , commentType    = CommentStart
                    }
            , author
            , requestedUsers = []
            }

collectChoices :: [VoteMaterialized] -> Map Choice (HashSet User)
collectChoices votes =
    Map.fromListWith
        (<>)
        [ (choice, HashSet.singleton voter)
        | VoteMaterialized{choice, voter} <- votes
        ]

selectList :: [Filter Issue] -> Handler [Entity Issue]
selectList filters =
    runDB do
        Entity _ User{userStellarAddress} <- requireAuth
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ ListIssues signerId
        Persist.selectList filters []

selectWithoutVoteFromUser :: Entity User -> Handler [Entity Issue]
selectWithoutVoteFromUser (Entity userId User{userStellarAddress}) =
    runDB do
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ ListIssues signerId
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

getContentForEdit :: IssueId -> Handler IssueContent
getContentForEdit issueId =
    runDB do
        userId <- requireAuthId
        issue@(Entity _ Issue{issueTitle, issueCurVersion}) <-
            getEntity404 issueId
        requireAuthz $ EditIssue issue userId
        versionId <-
            issueCurVersion
            ?| constraintFail "Issue.current_version must be valid"
        IssueVersion{issueVersionBody} <-
            get versionId
            ?|> constraintFail
                    "Issue.current_version must exist in IssueVersion table"
        pure IssueContent{title = issueTitle, body = issueVersionBody}

create :: IssueContent -> Handler IssueId
create IssueContent{title, body} = do
    now <- liftIO getCurrentTime
    Entity userId User{userStellarAddress} <- requireAuth
    runDB do
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ CreateIssue signerId
        let issue = Issue
                { issueApproval     = 0
                , issueTitle        = title
                , issueAuthor       = userId
                , issueOpen         = True
                , issueCommentNum   = 0
                , issueCreated      = now
                , issueCurVersion   = Nothing
                }
        issueId <- insert issue
        let version = IssueVersion
                { issueVersionIssue     = issueId
                , issueVersionBody      = body
                , issueVersionCreated   = now
                , issueVersionAuthor    = userId
                }
        versionId <- insert version
        update issueId [IssueCurVersion =. Just versionId]
        pure issueId

edit :: IssueId -> IssueContent -> Handler ()
edit issueId IssueContent{title, body} = do
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

closeReopen :: IssueId -> StateAction -> Handler ()
closeReopen issueId stateAction = do
    now <- liftIO getCurrentTime
    user <- requireAuthId
    runDB do
        issue <- getEntity404 issueId
        requireAuthz $ CloseReopenIssue issue user
        update issueId [IssueOpen =. newState]
        insert_
            Comment
                { commentAuthor  = user
                , commentCreated = now
                , commentMessage = ""
                , commentParent  = Nothing
                , commentIssue   = issueId
                , commentType
                }
  where
    newState =
        case stateAction of
            Close  -> False
            Reopen -> True
    commentType =
        case stateAction of
            Close  -> CommentClose
            Reopen -> CommentReopen
