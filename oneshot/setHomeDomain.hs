{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Named

import Stellar.Simple

main :: IO ()
main = do
    client <- getPublicClient ! defaults
    tx <-
        transactionBuilder account
        & op_setHomeDomain "veche.montelibero.org"
        & build client
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret tx
    Text.putStrLn $ envelope & xdrSerializeBase64T
  where
    account = Address "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
