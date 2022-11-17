{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Import.NoFoundation
    ( module X
    , module Import.NoFoundation
    ) where

-- prelude
import ClassyPrelude as X hiding (Handler, delete, for_, id, link, link2, on,
                           poll)

-- global
import CMarkGFM (commonmarkToHtml, extAutolink, extStrikethrough, extTable,
                 extTagfilter, optHardBreaks, optSmart)
import Control.Arrow as X ((>>>))
import Control.Monad.Logger as X (LoggingT, MonadLogger, MonadLoggerIO,
                                  logError, logInfo, logWarn, runLoggingT)
import Data.Aeson as X (FromJSON, ToJSON, Value, object, (.=))
import Data.Default as X (def)
import Data.Foldable as X (for_)
import Data.Function as X ((&))
import Data.Kind as X (Type)
import Data.Proxy as X (Proxy (Proxy))
import Data.Scientific as X (Scientific)
import Data.Tree as X (Forest, Tree (Node), unfoldForest)
import Data.Void as X (Void)
import Data.Yaml as X (array)
import Database.Persist as X (Entity (..), Key)
import Database.Persist.Sql as X (SqlPersistT)
import GHC.Stack as X (HasCallStack)
import Network.HTTP.Types as X (internalServerError500, status400)
import Text.Blaze.Html (preEscapedToHtml)
import UnliftIO (link)
import Yesod.Auth as X
import Yesod.Core as X (Fragment ((:#:)), HandlerFor, Html, HtmlUrl,
                        MonadHandler, PathPiece, TypedContent (TypedContent),
                        addHeader, cacheSeconds, defaultLayout, fromPathPiece,
                        getCurrentRoute, getYesod, hamlet, invalidArgs, julius,
                        lookupGetParam, lookupPostParams, notFound,
                        permissionDenied, redirect, requireCheckJsonBody,
                        respondSource, returnJson, sendChunkText,
                        sendResponseStatus, setTitle, setTitleI, toContent,
                        toHtml, toPathPiece, toWidget, typePlain, whamlet,
                        withUrlRenderer)
import Yesod.Core.Types as X (loggerSet)
import Yesod.Form as X (AForm, Enctype (UrlEncoded), Field (..),
                        FieldSettings (..), FormResult (FormSuccess), MForm,
                        Textarea (..), aopt, areq, fieldSettingsLabel,
                        generateFormPost, hiddenField, intField, ireq,
                        runFormPost, textField, textareaField)

-- project
import WithCallStack as X

-- component
import Authorization as X
import Database.Persist.Extra as X
import Form as X
import Model.Types as X
import Settings as X
import Settings.StaticFiles as X

inflect :: Int -> String -> String -> String
inflect 1 single _ = "1 " <> single
inflect n _ plural = show n <> " " <> plural

constraintFail :: MonadHandler m => Text -> m a
constraintFail msg =
    sendResponseStatus internalServerError500 $ "Constraint failed: " <> msg

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y
    | f x > f y = x
    | otherwise = y

(?|) :: Applicative f => Maybe a -> f a -> f a
Nothing ?| act = act
Just x  ?| _   = pure x

(?|>) :: Monad f => f (Maybe a) -> f a -> f a
m ?|> k = m >>= (?| k)

asyncLinked :: MonadUnliftIO m => m a -> m ()
asyncLinked = async >=> link

type EntitySet a = Map (Key a) a

renderMarkdown :: Text -> Html
renderMarkdown =
    preEscapedToHtml
    . commonmarkToHtml
        [optHardBreaks, optSmart]
        [extStrikethrough, extTable, extAutolink, extTagfilter]

identity :: a -> a
identity x = x
