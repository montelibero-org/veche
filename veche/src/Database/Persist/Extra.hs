{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Extra where

import ClassyPrelude.Yesod hiding (Proxy (Proxy))

import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Database.Persist.Sql (PersistFieldSql, sqlType)

-- | Provides Persist instances using JSON-encoding as a String.
-- Useful for enums.
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
    , BaseBackend (YesodPersistBackend app) ~ SqlBackend
    , PersistStoreWrite (YesodPersistBackend app)
    , PersistUniqueRead (YesodPersistBackend app)
    )
