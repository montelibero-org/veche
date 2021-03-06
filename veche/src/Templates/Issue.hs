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
    ( actionForm
    , closeReopenForm
    , editIssueForm
    , issueRequestTable
    , issueTable
    , newIssueForm
    , voteForm
    ) where

import Import

-- global
import GHC.Stack (HasCallStack)
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Model.Request (RequestMaterialized (..))
import Templates.Comment (commentAnchor)
import Types.Issue (IssueContent (..))

actionButton :: Text -> Text -> [Text] -> AForm Handler Void
actionButton value label classes =
    submitButtonReq SubmitButton{name = "action", value, label, classes}
    $> error "Void"

-- | Generate-only form; for its input, one must use 'actionForm'
closeReopenForm :: IssueId -> Bool -> Form Void
closeReopenForm issueId issueOpen =
    (bform
        if issueOpen then
            actionButton "close" "Close" ["btn-danger"]
        else
            actionButton "reopen" "Reopen" ["btn-success"]
    )
        {action = Just $ IssueR issueId}

-- | Generate-only form; for its input, one must use 'actionForm'
voteForm :: IssueId -> Form Void
voteForm issueId =
    BForm
        { aform = pure $ error "Void"
        , action = Just $ IssueR issueId
        , classes = ["form-inline"]
        , footer =
            [whamlet|
                <button .btn .btn-success name=action type=submit value=approve>
                    Approve
                <button .btn .btn-danger name=action type=submit value=reject>
                    Reject
            |]
        }

actionForm :: HasCallStack => Form Text
actionForm =
    -- We don't use "input form", because we need to check the CSRF token
    bform $
    areq
        textField
        FieldSettings
            { fsName    = Just "action"
            , fsId      = Nothing
            , fsAttrs   = undef
            , fsLabel   = undef
            , fsTooltip = undef
            }
        undef
  where
    undef :: HasCallStack => a
    undef = error "actionForm must be user with runFormPostB only"

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
                    <button .btn .btn-success
                            type=submit name=action value=edit>
                        Save
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
