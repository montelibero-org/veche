{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Forum (
    getForumIssues,
    getForumR,
    getForumsR,
) where

-- prelude
import Authorization
import Import

-- global
import Control.Monad.Except (throwError)
import Data.Map.Strict qualified as Map
import Database.Persist (selectList, (==.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Servant.Server qualified as Servant

-- component
import Genesis qualified
import Model.Forum qualified as Forum
import Model.Issue (Issue)
import Model.Issue qualified as Issue
import Model.User (maybeAuthzRoles)
import Templates.Issue (issueTable)

getForumR :: ForumId -> Handler Html
getForumR forumId = do
    (_, roles) <- maybeAuthzRoles
    mState <- lookupGetParam "state"
    let stateOpen = mState /= Just "closed"
    forumE@(_, Forum{title}) <- Forum.getEntity404 forumId
    issues <- Issue.listForumIssues forumE $ Just stateOpen
    (openIssueCount, closedIssueCount) <- Issue.countOpenAndClosed forumId
    let isAddForumIssueAllowed = isAllowed $ AddForumIssue forumE roles
    defaultLayout $(widgetFile "forum")

getForumIssues :: ConnectionPool -> ForumId -> Bool -> Servant.Handler [Issue]
getForumIssues pool forumId isOpen = do
    Forum{requireRole} <- Forum.get forumId ?| throwError Servant.err404
    when (isJust requireRole) $ throwError Servant.err403
    runSqlPoolIO pool $
        selectList [#forum ==. forumId, #open ==. isOpen] [] <&> map entityVal

runSqlPoolIO :: MonadIO io => ConnectionPool -> SqlPersistT IO a -> io a
runSqlPoolIO pool = liftIO . (`runSqlPool` pool)

getForumsR :: Handler Html
getForumsR = do
    (_, roles) <- maybeAuthzRoles
    let forums =
            filter (\f -> isAllowed $ ReadForum f roles) $
            Map.assocs Genesis.forums
    defaultLayout $(widgetFile "forums")
