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
    CommentInput (..),
    commentAnchor,
    commentForm,
    commentWidget,
) where

import Import

-- global
import Data.Set qualified as Set
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapHorizontalForm),
                              BootstrapGridOptions (ColSm), bfs,
                              renderBootstrap3)

-- component
import Model.Request (IssueRequestMaterialized (..))
import Templates.User (userNameText, userNameWidget)
import Types.Comment (CommentMaterialized (..))

commentWidget :: CommentMaterialized -> Widget
commentWidget CommentMaterialized{id, author, comment, requestedUsers} =
    $(widgetFile "comment")
  where
    Comment{commentMessage, commentType, commentCreated} = comment

commentAnchor :: CommentId -> Text
commentAnchor id = "comment" <> toPathPiece id

data CommentInput = CommentInput
    { issue        :: IssueId
    , message      :: Text
    , requestUsers :: Set UserId
    , provideInfo  :: Set RequestId
    }
    deriving Show

commentForm ::
    Maybe IssueId ->
    [IssueRequestMaterialized] ->
    (Html -> MForm Handler (FormResult CommentInput, Widget))
commentForm mIssueId activeRequests =
    renderBootstrap3
        (BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10))
        aform
  where

    aform = do
        issue <-
            areq
                hiddenField
                (bfs ("" :: Text)){fsName = Just "issue"}
                mIssueId
        message <-
            unTextarea <$>
            areq
                textareaField
                (bfs ("Comment" :: Text)){fsName = Just "message"}
                Nothing
        provideInfo <-
            aopt
                ( checkboxesFieldList'
                    [ (requestLabel r, id)
                    | r@IssueRequestMaterialized{id} <- activeRequests
                    ]
                )
                "Provide info for"{fsName = Just "provide"}
                Nothing
        pure
            CommentInput
                { issue
                , message
                , requestUsers = Set.empty
                , provideInfo = maybe Set.empty Set.fromList provideInfo
                }

requestLabel :: IssueRequestMaterialized -> Text
requestLabel IssueRequestMaterialized{requestor, comment} =
    userNameText user <> ": " <> commentMessage
  where
    Entity _ user = requestor
    Entity _ Comment{commentMessage} = comment

checkboxesFieldList' ::
    (Eq a, PathPiece a) => [(Text, a)] -> Field (HandlerFor site) [a]
checkboxesFieldList' opts = (checkboxesFieldList opts){fieldView} where
    fieldView _id name attrs _val _isReq =
        [whamlet|
            $forall (display, value) <- opts
                <div .checkbox>
                    <label>
                        <input type=checkbox name=#{name}
                            value=#{toPathPiece value} *{attrs}>
                        #{display}
        |]
