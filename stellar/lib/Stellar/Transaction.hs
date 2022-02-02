{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Stellar.Transaction where

import Stellar.Account (Account (..))
import Stellar.Memo (Memo (..))

data Transaction = Transaction{sourceAccount :: Account, memo :: Memo}
    deriving (Show)

makeTransaction :: Account -> Transaction
makeTransaction sourceAccount = Transaction{sourceAccount, memo = MemoNone}
