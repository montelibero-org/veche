{-# OPTIONS -Wno-orphans #-} -- Persist instances for external types
{-# OPTIONS -Wno-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import ClassyPrelude

import Data.Decimal (Decimal)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Database.Persist.TH (derivePersistField, mkMigrate, mkPersist,
                            mpsConstraintLabelModifier, mpsFieldLabelModifier,
                            persistFileWith, share, sqlSettings)
import Stellar.Horizon.Types (Asset (Asset), TxId)
import Stellar.Horizon.Types qualified as Stellar

import Model.Types (Choice, CommentType, ForumId, Poll, Role,
                    StellarMultiSigAddress)

deriving newtype instance PersistField    Asset
deriving newtype instance PersistFieldSql Asset

deriving newtype instance PersistField    TxId
deriving newtype instance PersistFieldSql TxId

derivePersistField "Decimal"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
$(  let lowerFirst t =
            case uncons t of
                Just (a, b) -> cons (charToLower a) b
                Nothing -> t
        mpsConstraintLabelModifier entity field =
            entity ++ "_" ++ lowerFirst field
        mpsFieldLabelModifier _entity = \case
            "Data"  -> "data_"
            "Type"  -> "type_"
            field   -> lowerFirst field
        settings =
            sqlSettings{mpsConstraintLabelModifier, mpsFieldLabelModifier}
    in
    share
        [ mkPersist settings
        , mkMigrate "migrateAll"
        ]
        $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
    )
