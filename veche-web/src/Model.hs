{-# OPTIONS -Wno-orphans #-} -- Persist instances for external types
{-# OPTIONS -Wno-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

-- prelude
import ClassyPrelude

-- global
import Data.Aeson (FromJSON)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Database.Persist (PersistField)
import Database.Persist.Extra (JsonString (JsonString))
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (PersistFieldSql)
import Database.Persist.Sql qualified
import Database.Persist.TH (derivePersistFieldJSON, mkMigrate, mkPersist,
                            mpsConstraintLabelModifier, mpsFieldLabelModifier,
                            persistFileWith, share, sqlSettings)

-- project
import Stellar.Simple (Asset, Operation, TransactionOnChain, TxId (TxId))
import Stellar.Simple qualified as Stellar

-- component
import Model.Types (Choice, CommentType, ForumId, Poll, Role,
                    StellarMultiSigAddress, TransactionBin)

deriving via JsonString Asset instance PersistField    Asset
deriving via JsonString Asset instance PersistFieldSql Asset

derivePersistFieldJSON "Scientific"

deriving newtype instance PersistField    TxId
deriving newtype instance PersistFieldSql TxId

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
        [mkPersist settings, mkMigrate "migrateAll"]
        $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
    )

data Corrections = Corrections{exclude :: Set TxId, include :: Map TxId IssueId}
    deriving (FromJSON, Generic)

type CorrectionsFile = Corrections

type TransactionsOnChain = [TransactionOnChain]

type EscrowFile = TransactionsOnChain

data EscrowStat = EscrowStat
    { balances      :: Map IssueId (Map Asset Scientific)
    , excluded      :: [TransactionOnChain]
    , unknownOps    :: [Operation]
    , unknownTxs    :: [TransactionOnChain]
    }

instance Semigroup EscrowStat where
    EscrowStat b1 e1 o1 t1 <> EscrowStat b2 e2 o2 t2 =
        EscrowStat
        { balances      = Map.unionWith (Map.unionWith (+)) b1 b2
        , excluded      = e1 <> e2
        , unknownOps    = o1 <> o2
        , unknownTxs    = t1 <> t2
        }

instance Monoid EscrowStat where
    mempty =
        EscrowStat
        { balances      = mempty
        , excluded      = mempty
        , unknownOps    = mempty
        , unknownTxs    = mempty
        }
