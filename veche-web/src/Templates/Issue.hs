{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Yesod.Core (getMessageRender)
import Yesod.Form (Option (Option), mkOptionList, radioField)
import Yesod.Form qualified
import Yesod.Form.Bootstrap5 (bfs)

-- component
import Model.Comment (Comment (Comment))
import Model.Comment qualified
import Model.Issue (Issue (Issue), IssueContent (IssueContent), IssueId)
import Model.Issue qualified
import Model.Request (RequestMaterialized (RequestMaterialized))
import Model.Request qualified
import Templates.Comment (commentAnchor)

closeReopenButton :: IssueId -> Bool -> Widget
closeReopenButton issueId issueIsOpen = do
    mr <- getMessageRender
    if issueIsOpen then
        actionButton
            (IssueCloseR issueId)
            ["btn-danger"]
            (mr MsgIssueClose)
            True
    else
        actionButton
            (IssueReopenR issueId)
            ["btn-success"]
            (mr MsgIssueReopen)
            True

voteButtons :: Bool -> IssueId -> Choice -> Widget
voteButtons isEnabled issueId currentChoice =
    [whamlet|
        <div .btn-group role=group>
            ^{buttonApprove}
            ^{buttonAgainst}
            $if currentChoice /= Abstain
                <div .btn-group role=group>
                    <button type=button .btn.btn-outline-primary.dropdown-toggle
                            data-bs-toggle=dropdown
                            aria-haspopup=true aria-expanded=false>
                        <span .caret>
                    <ul .dropdown-menu>
                        <li>
                            <a .dropdown-item onclick="submitPostForm('@{IssueVoteR issueId Abstain}')"
                                    href="#">
                                ◯ Abstain
    |]
  where

    buttonApprove =
        actionButton
            (IssueVoteR issueId Approve)
            ["btn-success"]
            "👍 Approve"
            (isEnabled && currentChoice /= Approve)

    buttonAgainst =
        actionButton
            (IssueVoteR issueId Reject)
            ["btn-danger"]
            "👎 Against"
            (isEnabled && currentChoice /= Reject)

issueForm :: EntityForum -> Maybe IssueContent -> Form IssueContent
issueForm (forumId, forum) previousContent = (bform aform){header} where

    Forum   { enableAttachTx
            , enableContacts
            , enablePriceOffer
            , pollOptions = enabledPollOptions
            , title = forumTitle
            } =
        forum

    header =
        [whamlet|
            <div .row .mb-3>
                <label .col-sm-2 .col-form-label .fw-bold>_{MsgIssueForum}
                <div .col-sm-10 .form-control-static>
                    <a href=@{ForumR forumId}>#{forumTitle}
        |]

    (           previousAttachmentTx
            ,   previousBody
            ,   previousContacts
            ,   previousPoll
            ,   previousPriceOffer
            ,   previousTitle
            ) =
        case previousContent of
            Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            Just    IssueContent
                    {attachmentTx, body, contacts, poll, priceOffer, title}
                ->  ( Just $ attachmentTx <&> \(TransactionB64 t) -> Textarea t
                    , Just $ Textarea body
                    , Textarea <$> contacts
                    , Just poll
                    , Textarea <$> priceOffer :: Maybe Textarea
                    , Just title
                    )

    aform = do
        title <-
            areq
                textField
                (bfs MsgIssueTitle){fsName = Just "title"}
                previousTitle
        body <-
            unTextarea <$>
            areq
                textareaField
                (   (bfs MsgIssueMessage){fsName = Just "body"}
                    & addAttr "style" "height: 20em;"
                )
                previousBody
        priceOffer <-
            whenMay enablePriceOffer $
                fmap unTextarea
                <$> aopt
                        textareaField
                        (bfs MsgIssuePriceOffer){fsName = Just "priceOffer"}
                        (Just previousPriceOffer)
        contacts <-
            whenMay enableContacts $
                Just . unTextarea
                <$> areq
                        textareaField
                        (bfs MsgIssueContacts){fsName = Just "contacts"}
                        previousContacts
        poll <-
            whenMay (not $ null enabledPollOptions) $
            areq
                (radioField $ mkOptionList <$> pollOptions)
                (fieldSettingsLabel MsgIssuePoll){fsName = Just "poll"}
                previousPoll
        attachmentTx <-
            whenMay enableAttachTx $
                fmap (TransactionB64 . unTextarea)
                <$> aopt
                        textareaField
                        (fieldSettingsLabel MsgIssueAttachmentTx)
                            {fsName = Just "attachmentTx"}
                        previousAttachmentTx
        pure IssueContent{attachmentTx, body, contacts, poll, priceOffer, title}

    pollOptions = do
        mr <- getMessageRender
        pure $
            Option
                { optionDisplay = mr MsgPollDisabled
                , optionInternalValue = Nothing
                , optionExternalValue = "null"
                }
            :   [ Option
                    { optionDisplay = mr $ display opt
                    , optionInternalValue = Just opt
                    , optionExternalValue = apiValue opt
                    }
                | opt <- enabledPollOptions
                ]

    display = \case
        ByAmountOfFcm   -> MsgPollByAmountOfFcm
        ByAmountOfVeche -> MsgPollByAmountOfVeche
        ByMtlAmount     -> MsgPollMtlShare
        BySignerWeight  -> MsgPollMtlSignerWeight

    apiValue = \case
        ByAmountOfFcm   -> "FCM"
        ByAmountOfVeche -> "VECHE"
        ByMtlAmount     -> "MTL"
        BySignerWeight  -> "MTL_FUND_SIGNER_WEIGHT"

    whenMay cond action
        | cond      = action
        | otherwise = pure Nothing

editIssueForm ::
    EntityForum -> IssueId -> Maybe IssueContent -> Form IssueContent
editIssueForm forumE issueId previousContent =
    (issueForm forumE previousContent)
        { action = Just $ IssueR issueId
        , footer =
            [whamlet|
                <div .d-flex>
                    <div .me-auto>
                        <a .btn .btn-outline-primary href=@{IssueR issueId}>_{MsgCancel}
                    <div>
                        <button .btn .btn-success type=submit>_{MsgSave}
            |]
        }

newIssueForm :: EntityForum -> Form IssueContent
newIssueForm forum@(forumId, _) =
    (issueForm forum Nothing)
        { action = Just $ ForumIssuesR forumId
        , footer =
            [whamlet|
                <div .row>
                    <div .offset-sm-2.col-sm-10>
                        <button type=submit .btn .btn-success>_{MsgIssuePublish}
            |]
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
