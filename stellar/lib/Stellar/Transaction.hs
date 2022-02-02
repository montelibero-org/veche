{-# LANGUAGE NamedFieldPuns #-}

module Stellar.Transaction where

import Stellar.Account (Account (..))
import Stellar.Memo (Memo (..))
import Stellar.XDR (Transaction)

data Transaction = Transaction{sourceAccount :: Account, memo :: Memo}
    deriving (Show)

makeTransaction :: Account -> Transaction
makeTransaction sourceAccount = Transaction{sourceAccount, memo = MemoNone}
