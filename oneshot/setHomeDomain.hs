{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Stellar.Simple

main :: IO ()
main = do
    t <-
        transactionBuilder account
        & addSetHomeDomain "veche.montelibero.org"
        & build
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    Text.putStrLn $ t & signWithSecret secret & xdrSerializeBase64T
  where
    account = Address "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
