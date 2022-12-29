{-# LANGUAGE OverloadedStrings #-}

module Import.Extra where

-- global
import CMarkGFM (commonmarkToHtml, extAutolink, extStrikethrough, extTable,
                 extTagfilter, optHardBreaks, optSmart)
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Database.Persist (Key)
import GHC.Stack (HasCallStack)
import Network.HTTP.Types as X (internalServerError500)
import Text.Blaze.Html (Html, preEscapedToHtml)
import UnliftIO (MonadUnliftIO, async, link)
import Yesod.Core (MonadHandler, sendResponseStatus)

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

decodeUtf8Throw :: HasCallStack => ByteString -> Text
decodeUtf8Throw = either (error . show) identity . Text.decodeUtf8'

unwrap :: Coercible a b => (a -> b) -> b -> a
unwrap _ = coerce

