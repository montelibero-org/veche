module Stellar.Memo where

import Data.ByteString (ByteString)

data Memo = MemoNone | MemoText ByteString
    deriving (Show)
