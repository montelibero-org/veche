{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Issue (
    IssueMaterialized (..),
    countOpenAndClosed,
    getContentForEdit,
    load,
    selectList,
    selectWithoutVoteFromUser,
) where

import Import hiding (selectList)

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

countOpenAndClosed :: Handler (Int, Int)
countOpenAndClosed = do
    counts' :: [(Single Bool, Single Int)] <-
        runDB $ rawSql "SELECT open, COUNT(*) FROM issue GROUP BY open" []
    let counts = coerce counts' :: [(Bool, Int)]
    pure
        ( findWithDefault 0 True  counts
        , findWithDefault 0 False counts
        )

loadComments :: MonadIO m => IssueId -> SqlPersistT m [CommentMaterialized]
loadComments issueId = do
    comments <-
        rawSql
            "SELECT ??, ??\
            \ FROM comment, user ON comment.author == user.id\
            \ WHERE comment.issue == ?"
            [toPersistValue issueId]
    pure
        [ CommentMaterialized{comment, author}
        | (Entity _ comment, Entity _ author) <- comments
        ]

loadVotes :: MonadIO m => IssueId -> SqlPersistT m [VoteMaterialized]
loadVotes issueId = do
    votes <-
        rawSql
            "SELECT ??, ??\
            \ FROM vote, user ON vote.user == user.id\
            \ WHERE vote.issue == ?"
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

        issue@Issue{issueAuthor, issueCreated, issueCurVersion} <-
            get404 issueId
        versionId <-
            issueCurVersion
            ?| constraintFail "Issue.current_version must be valid"
        author <-
            get issueAuthor
            ?|> constraintFail "Issue.author must exist in User table"
        comments' <- loadComments issueId
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

(?|) :: Applicative f => Maybe a -> f a -> f a
Nothing ?| action   = action
Just x  ?| _        = pure x

(?|>) :: Monad f => f (Maybe a) -> f a -> f a
m ?|> k = m >>= (?| k)

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
