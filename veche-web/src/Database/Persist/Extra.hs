{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Extra where

import ClassyPrelude

import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Database.Persist (Entity (Entity), Key, OnlyOneUniqueKey, PersistEntity,
                         PersistEntityBackend, PersistField, PersistStoreWrite,
                         PersistUniqueRead, PersistValue (PersistText),
                         SqlType (SqlString), Unique, Update, fromPersistValue,
                         getBy, toPersistValue, upsert)
import Database.Persist.Sql (PersistFieldSql, SqlBackend, SqlPersistT, sqlType)
import Yesod.Auth (YesodAuthPersist)
import Yesod.Core (MonadHandler, permissionDenied)
import Yesod.Persist (YesodPersist, YesodPersistBackend, get404)

-- | Provides Persist instances using JSON-encoding as a String.
-- Useful for enums.
type role JsonString representational
newtype JsonString a = JsonString a

instance (FromJSON a, ToJSON a, Typeable a) => PersistField (JsonString a) where
    toPersistValue (JsonString a) =
        case toJSON a of
            String s -> toPersistValue s
            j ->
                error $
                    "toPersistValue @" ++ show (typeRep $ Proxy @a)
                    ++ ": Expected String, but got " ++ show j
    fromPersistValue = \case
        PersistText a ->
            bimap Text.pack JsonString $ parseEither parseJSON $ String a
        pv ->
            Left $
                "fromPersistValue @" <> tshow (typeRep $ Proxy @a)
                <> ": Expected PersistText, but got " <> tshow pv

instance (FromJSON a, ToJSON a, Typeable a) => PersistFieldSql (JsonString a)
    where

    sqlType _ = SqlString

type PersistSql app =
    ( YesodPersist app
    , YesodPersistBackend app ~ SqlBackend
    , YesodAuthPersist app
    , PersistStoreWrite (YesodPersistBackend app)
    , PersistUniqueRead (YesodPersistBackend app)
    )

getBy403 ::
    ( MonadHandler m
    , PersistEntity val
    , PersistEntityBackend val ~ SqlBackend
    ) =>
    Unique val -> SqlPersistT m (Entity val)
getBy403 key = do
    mres <- getBy key
    case mres of
        Nothing  -> permissionDenied "Not authorized"
        Just res -> return res

getEntity404 ::
    (MonadIO m, PersistEntity val, PersistEntityBackend val ~ SqlBackend) =>
    Key val -> SqlPersistT m (Entity val)
getEntity404 key = Entity key <$> get404 key

upsert_ ::
    (MonadIO m, OnlyOneUniqueKey val, PersistEntityBackend val ~ SqlBackend) =>
    val -> [Update val] -> SqlPersistT m ()
upsert_ record updates = void $ upsert record updates
