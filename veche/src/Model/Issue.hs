{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Issue (
    IssueMaterialized (..),
    getContentForEdit,
    load,
    selectWithoutVoteFromUser,
) where

import Import.NoFoundation

-- global
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Database.Persist.Sql (rawSql)

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

load ::
    (PersistSql app, AuthId app ~ UserId, AuthEntity app ~ User) =>
    IssueId -> HandlerFor app IssueMaterialized
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

selectWithoutVoteFromUser ::
    PersistSql app => Entity User -> HandlerFor app [Entity Issue]
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

getContentForEdit ::
    (PersistSql app, AuthId app ~ UserId) =>
    IssueId -> HandlerFor app IssueContent
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
