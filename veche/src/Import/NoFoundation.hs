{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Import.NoFoundation
    ( module Import
    , module Import.NoFoundation
    ) where

import ClassyPrelude.Yesod as Import hiding (Request)
import Data.Function as Import ((&))
import Data.Kind as Import (Type)
import Yesod.Auth as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Default.Config2 as Import

import Authorization as Import
import Database.Persist.Extra as Import
import Form as Import
import Model as Import
import Model.Types as Import
import Settings as Import
import Settings.StaticFiles as Import

inflect :: Int -> String -> String -> String
inflect 1 single _ = "1 " <> single
inflect n _ plural = show n <> " " <> plural

constraintFail :: MonadHandler m => Text -> m a
constraintFail msg =
    sendResponseStatus internalServerError500 $ "Constraint failed: " <> msg

getBy403 ::
    ( MonadHandler m
    , PersistRecordBackend val backend
    , PersistUniqueRead backend
    ) =>
    Unique val -> ReaderT backend m (Entity val)
getBy403 key = do
    mres <- getBy key
    case mres of
        Nothing  -> permissionDenied "Not authorized"
        Just res -> return res

getEntity404 ::
    (MonadIO m, PersistStoreRead backend, PersistRecordBackend val backend) =>
    Key val -> ReaderT backend m (Entity val)
getEntity404 key = Entity key <$> get404 key

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y
    | f x > f y = x
    | otherwise = y

upsert_ ::
    ( MonadIO m
    , PersistUniqueWrite backend
    , OnlyOneUniqueKey record
    , PersistEntityBackend record ~ BaseBackend backend
    ) =>
    record -> [Update record] -> ReaderT backend m ()
upsert_ record updates = void $ upsert record updates
