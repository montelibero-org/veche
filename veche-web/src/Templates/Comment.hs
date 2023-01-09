{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.Comment (
    commentAnchor,
    commentForm,
    commentForestWidget,
) where

import Import

-- global
import Data.Set qualified as Set
import Data.Time (rfc822DateFormat)
import Yesod.Form.Bootstrap3 (bfs)

-- component
import Model.Comment (Comment (Comment), CommentId, CommentInput (CommentInput),
                      CommentMaterialized (CommentMaterialized))
import Model.Comment qualified
import Model.Issue (IssueId)
import Model.Request (IssueRequestMaterialized (IssueRequestMaterialized))
import Model.Request qualified
import Templates.User (userNameText, userNameWidget)

commentForestWidget :: Forest CommentMaterialized -> Widget
commentForestWidget comments =
    [whamlet|
        <ul>
            $forall comment <- comments
                ^{commentWidget comment}
    |]

commentWidget :: Tree CommentMaterialized -> Widget
commentWidget (Node comment' subComments) = $(widgetFile "comment") where
    CommentMaterialized{id, author, comment, requestedUsers} = comment'
    Entity authorId commentAuthor = author
    Comment{message, type_, created} = comment
    isAlive = type_ /= CommentTombstone
    createdTime = formatTime defaultTimeLocale rfc822DateFormat created

commentAnchor :: CommentId -> Text
commentAnchor id = "comment" <> toPathPiece id

commentForm ::
    -- | Just if known (for rendering), Nothing if unknown (for reading)
    Maybe (IssueId, [IssueRequestMaterialized]) ->
    (Html -> MForm Handler (FormResult CommentInput, Widget))
commentForm = renderFormHorizontal . commentAForm

commentAForm ::
    -- | Just if known (for rendering), Nothing if unknown (for reading)
    Maybe (IssueId, [IssueRequestMaterialized]) ->
    AForm Handler CommentInput
commentAForm mParams = do
    issue   <- areq hiddenField ""{fsName = Just "issue"} mIssueId
    parent  <-
        aopt
            hiddenField
            ""{fsId = Just "comment_parent", fsName = Just "parent"}
            Nothing
    message <-
        unTextarea <$>
        areq
            textareaField
            (bfs MsgIssueComment)
                {fsId = Just "comment_message", fsName = Just "message"}
            Nothing
    provideInfo <-
        case mActiveRequests of
            Just [] -> pure []
            _ ->
                areq
                    ( checkboxesFieldList'
                        [ (requestLabel r, id)
                        | r@IssueRequestMaterialized{id} <-
                            fromMaybe [] mActiveRequests
                        ]
                    )
                    (fieldSettingsLabel MsgCommentProvideInfo)
                        {fsName = Just "provide"}
                    Nothing
    pure
        CommentInput
            { issue
            , message
            , requestUsers  = Set.empty
            , provideInfo   = Set.fromList provideInfo
            , parent
            }
  where
    mIssueId        = fst <$> mParams
    mActiveRequests = snd <$> mParams

requestLabel :: IssueRequestMaterialized -> Text
requestLabel IssueRequestMaterialized{requestor, comment} =
    userNameText user <> ": " <> message
  where
    Entity _ user = requestor
    Entity _ Comment{message} = comment

checkboxesFieldList' ::
    PathPiece a => [(Text, a)] -> Field (HandlerFor site) [a]
checkboxesFieldList' opts =
    Field{fieldParse, fieldView, fieldEnctype = UrlEncoded}
  where

    fieldParse optlist _ =
        pure
            case traverse fromPathPiece optlist of
                Nothing  -> Left "Error parsing values"
                Just res -> Right $ Just res

    fieldView _id name attrs _val _isReq =
        [whamlet|
            $forall (display, value) <- opts
                <div .checkbox>
                    <label>
                        <input type=checkbox name=#{name}
                            value=#{toPathPiece value} *{attrs}>
                        #{display}
        |]
