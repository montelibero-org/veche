{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Stellar.Transaction where

import Network.ONCRPC.XDR (XDR, xdrPut)
import Network.ONCRPC.XDR qualified

import Stellar.Account (Account (..))
import Stellar.Memo (Memo (..))

data Transaction = Transaction{sourceAccount :: Account, memo :: Memo}
    deriving (Show)

makeTransaction :: Account -> Transaction
makeTransaction sourceAccount = Transaction{sourceAccount, memo = MemoNone}

instance XDR Transaction where
    xdrType _ = "Transaction"

    xdrPut Transaction{..} = do
        xdrPut sourceAccountId
        -- self.fee.pack(packer)
        -- self.seq_num.pack(packer)
        -- if self.time_bounds is None:
        --     packer.pack_uint(0)
        -- else:
        --     packer.pack_uint(1)
        --     if self.time_bounds is None:
        --         raise ValueError("time_bounds should not be None.")
        --     self.time_bounds.pack(packer)
        -- self.memo.pack(packer)
        -- packer.pack_uint(len(self.operations))
        -- for operations_item in self.operations:
        --     operations_item.pack(packer)
        -- self.ext.pack(packer)
      where
        Account{account = sourceAccountId, sequence} = sourceAccount

    xdrGet = undefined
