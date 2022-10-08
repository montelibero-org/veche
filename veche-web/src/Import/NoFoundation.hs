{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Import.NoFoundation
    ( module X
    , module Import.NoFoundation
    ) where

import ClassyPrelude as X hiding (Handler, delete, id, poll)

import Control.Arrow as X ((>>>))
import Data.Aeson as X (FromJSON, ToJSON, Value, object, (.=))
import Data.Default as X (def)
import Data.Function as X ((&))
import Data.Kind as X (Type)
import Data.Proxy as X (Proxy (Proxy))
import Data.Tree as X (Forest, Tree (Node), unfoldForest)
import Data.Void as X (Void)
import Data.Yaml as X (array)
import Database.Persist as X (Entity (..), Key)
import Database.Persist.Sql as X (SqlPersistT)
import Network.HTTP.Types as X (internalServerError500, status400)
import Yesod.Auth as X
import Yesod.Core as X (Fragment ((:#:)), HandlerFor, Html, HtmlUrl,
                        MonadHandler, PathPiece, TypedContent (TypedContent),
                        addHeader, cacheSeconds, defaultLayout, fromPathPiece,
                        getCurrentRoute, hamlet, invalidArgs, julius,
                        lookupGetParam, lookupPostParams, permissionDenied,
                        redirect, requireCheckJsonBody, respondSource,
                        returnJson, sendChunkText, sendResponseStatus, setTitle,
                        toContent, toHtml, toPathPiece, toWidget, typePlain,
                        whamlet, withUrlRenderer)
import Yesod.Core.Types as X (loggerSet)
import Yesod.Form as X (AForm, Enctype (UrlEncoded), Field (..),
                        FieldSettings (..), FormResult (FormSuccess), MForm,
                        Textarea (..), aopt, areq, generateFormPost,
                        hiddenField, runFormPost, textField, textareaField)

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
