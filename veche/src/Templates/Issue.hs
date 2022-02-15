{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Templates.Issue where

import Import

-- global
import GHC.Stack (HasCallStack)
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Issue (IssueContent (..))

actionButton :: Text -> Text -> [Text] -> AForm Handler Void
actionButton value label extraClasses =
    submitButtonReq SubmitButton{name = "action", value, label, extraClasses}
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
    (bform
        (  actionButton "approve" "Approve" ["btn-success"]
        *> actionButton "reject"  "Reject"  ["btn-danger"]
        )
    )
        {action = Just $ IssueR issueId, extraClasses = ["form-inline"]}

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
