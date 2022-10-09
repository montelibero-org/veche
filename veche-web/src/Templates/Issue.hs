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
import Yesod.Form (FormMessage (MsgSelectNone), OptionList, optionsPairs,
                   withRadioField)
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Model.Comment (Comment (Comment))
import Model.Comment qualified
import Model.Forum (Forum (Forum))
import Model.Forum qualified
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

voteButtons :: Bool -> IssueId -> Widget
voteButtons isEnabled issueId = do
    actionButton
        (IssueVoteR issueId Approve) ["btn-success"] "Approve" isEnabled
    actionButton (IssueVoteR issueId Reject) ["btn-danger"] "Reject" isEnabled

radioFieldList :: Eq a => [(Text, a)] -> Field Handler a
radioFieldList = radioField . optionsPairs

-- TODO wait yesod-form 1.7.3
radioField :: Eq a => Handler (OptionList a) -> Field Handler a
radioField =
    withRadioField
        (\theId optionWidget ->
            [whamlet|
                $newline never
                <div .radio>
                    <label for=#{theId}-none>
                        <div>
                            ^{optionWidget}
                            _{MsgSelectNone}
            |]
        )
        (\theId value _isSel text optionWidget ->
            [whamlet|
                $newline never
                <div .radio>
                    <label for=#{theId}-#{value}>
                        <div>
                            ^{optionWidget}
                            \#{text}
            |]
        )

issueForm :: Entity Forum -> Maybe IssueContent -> Form IssueContent
issueForm (Entity forumId Forum{title = forumTitle}) previousContent =
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
                    [ ("Disabled",                  Nothing             )
                    , ("Weighted by signer weight", Just BySignerWeight )
                    ]
                )
                "Poll"{fsName = Just "poll"}
                (Just do IssueContent{poll} <- previousContent; poll)
        pure IssueContent{body, poll, title}

editIssueForm ::
    Entity Forum -> IssueId -> Maybe IssueContent -> Form IssueContent
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

newIssueForm :: Entity Forum -> Form IssueContent
newIssueForm forum@(Entity forumId _) =
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
