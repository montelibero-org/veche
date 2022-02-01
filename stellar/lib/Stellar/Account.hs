module Stellar.Account where

import Data.ByteString (ByteString)

data Account = Account{account :: ByteString, sequence :: Integer}
    deriving (Show)
