{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Text.Hamlet (shamlet)
import Yesod.Form (radioFieldList)
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Model.Comment (Comment (Comment))
import Model.Comment qualified
import Model.Issue (Issue (Issue), IssueContent (IssueContent), IssueId)
import Model.Issue qualified
import Model.Request (RequestMaterialized (RequestMaterialized))
import Model.Request qualified
import Templates.Comment (commentAnchor)

closeReopenButton :: IssueId -> Bool -> Widget
closeReopenButton issueId issueIsOpen
    | issueIsOpen =
        actionButton (IssueCloseR  issueId) ["btn-danger"] "Close" True
    | otherwise =
        actionButton (IssueReopenR issueId) ["btn-success"] "Reopen" True

voteButtons :: Bool -> IssueId -> Choice -> Widget
voteButtons isEnabled issueId currentChoice = do
    actionButton
        (IssueVoteR issueId Approve)
        ["btn-success"]
        [shamlet|
            <span .glyphicon.glyphicon-thumbs-up aria-hidden=true>
            Approve
        |]
        (isEnabled && currentChoice /= Approve)
    actionButton
        (IssueVoteR issueId Reject)
        ["btn-danger"]
        [shamlet|
            <span .glyphicon.glyphicon-thumbs-down aria-hidden=true>
            Against
        |]
        (isEnabled && currentChoice /= Reject)
    actionButton
        (IssueVoteR issueId Abstain)
        ["btn-default"]
        [shamlet|
            <span .glyphicon.glyphicon-unchecked aria-hidden=true>
            Abstain
        |]
        (isEnabled && currentChoice /= Abstain)

issueForm :: EntityForum -> Maybe IssueContent -> Form IssueContent
issueForm (forumId, Forum{title = forumTitle}) previousContent =
    (bform aform){header}
  where
    header =
        [whamlet|
            <div .form-group>
                <label .col-sm-2 .control-label>Forum
                <div .col-sm-10 .form-control-static>
                    <a href=@{ForumR forumId}>#{forumTitle}
        |]
    aform = do
        title <-
            areq
                textField
                (bfs ("Title" :: Text)){fsName = Just "title"}
                (previousContent <&> \IssueContent{title} -> title)
        body <-
            unTextarea <$>
            areq
                textareaField
                (bfs ("Message" :: Text)){fsName = Just "body"}
                (previousContent <&> \IssueContent{body} -> Textarea body)
        poll <-
            areq
                (radioFieldList
                    [ ("Disabled" :: Text,          Nothing             )
                    , ("Weighted by signer weight", Just BySignerWeight )
                    ]
                )
                "Poll"{fsName = Just "poll"}
                (Just do IssueContent{poll} <- previousContent; poll)
        pure IssueContent{body, poll, title}

editIssueForm ::
    EntityForum -> IssueId -> Maybe IssueContent -> Form IssueContent
editIssueForm forumE issueId previousContent =
    (issueForm forumE previousContent)
        { action = Just $ IssueR issueId
        , footer =
            [whamlet|
                <div .pull-left>
                    <a .btn .btn-default href=@{IssueR issueId}>Cancel
                <div .pull-right>
                    <button .btn .btn-success type=submit>Save
            |]
        }

newIssueForm :: EntityForum -> Form IssueContent
newIssueForm forum@(forumId, _) =
    (issueForm forum Nothing)
        { action = Just $ ForumIssuesR forumId
        , footer =
            [whamlet|<button type=submit .btn .btn-success>Start dicussion|]
        }

issueTable :: [Entity Issue] -> Widget
issueTable issues = $(widgetFile "issue-table")

issueTableRow :: Entity Issue -> Widget
issueTableRow (Entity issueId Issue{approval, commentNum, poll, title}) =
    $(widgetFile "issue-table-row")
  where
    approvalPercent = round $ approval * 100 :: Int

issueRequestTable :: [RequestMaterialized] -> Widget
issueRequestTable requests = $(widgetFile "issue-request-table")

issueRequestTableRow :: RequestMaterialized -> Widget
issueRequestTableRow RequestMaterialized{issue, comment} =
    $(widgetFile "issue-request-table-row")
  where
    Entity issueId Issue{title} = issue
    Entity commentId Comment{message} = comment
