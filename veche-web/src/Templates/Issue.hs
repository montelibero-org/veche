{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.Issue
    ( closeReopenButton
    , editIssueForm
    , issueRequestTable
    , issueTable
    , newIssueForm
    , voteButtons
    ) where

import Import

-- global
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Model.Request (RequestMaterialized (..))
import Templates.Comment (commentAnchor)
import Types.Issue (IssueContent (..))

closeReopenButton :: IssueId -> Bool -> HtmlUrl (Route App)
closeReopenButton issueId issueOpen
    | issueOpen =
        [hamlet|<button .btn .btn-danger post=@{IssueCloseR issueId}>Close|]
    | otherwise =
        [hamlet|<button .btn .btn-success post=@{IssueReopenR issueId}>Reopen|]

voteButtons :: IssueId -> HtmlUrl (Route App)
voteButtons issueId =
    [hamlet|
        <button .btn .btn-success post=@{IssueVoteR issueId Approve}>Approve
        <button .btn .btn-danger post=@{IssueVoteR issueId Reject}>Reject
    |]

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
                    <button .btn .btn-success type=submit>Save
            |]
        }

newIssueForm :: Form IssueContent
newIssueForm =
    (issueForm Nothing)
        { action = Just IssuesR
        , footer =
            [whamlet|<button type=submit .btn .btn-success>Start dicussion|]
        }

issueTable :: [Entity Issue] -> Widget
issueTable issues = $(widgetFile "issue-table")

issueTableRow :: Entity Issue -> Widget
issueTableRow (Entity issueId Issue{..}) = $(widgetFile "issue-table-row")
  where
    approvalPercent = round $ issueApproval * 100 :: Int

issueRequestTable :: [RequestMaterialized] -> Widget
issueRequestTable requests = $(widgetFile "issue-request-table")

issueRequestTableRow :: RequestMaterialized -> Widget
issueRequestTableRow RequestMaterialized{issue, comment} =
    $(widgetFile "issue-request-table-row")
  where
    Entity issueId Issue{issueTitle} = issue
    Entity commentId Comment{commentMessage} = comment
