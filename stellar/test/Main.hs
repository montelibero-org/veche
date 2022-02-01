{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Stellar (Account (..), Memo (..), Transaction (..), makeTransaction,
                xdrSerialize)

main :: IO ()
main =
    defaultMain $
    testCase "smoke" $
    xdrSerialize tx @?= testUnsingedTx
  where
    tx = (makeTransaction (Account testPublicKey 0)){memo = MemoText "log in"}

testPublicKey :: ByteString
testPublicKey = "GDLNZNS3HM3I3OAQKYV5WBACXCUU7JWEQLGGCAEV7E4LA4BY2XFOLEWX"

-- testSecretKey = "SA3KFUDKK5YDG2X2OKLW26JSTPGPUL6DOODIYZ4A2C4ZI3Y5RG2YJQVW"

testUnsingedTx :: ByteString
testUnsingedTx =
    "AAAAAgAAAADW3LZbOzaNuBBWK9sEAripT6bEgsxhAJX5OLBwONXK5QAAAAAAAAAA\
    \AAAAAQAAAAAAAAABAAAAEkxvZ2dpbmcgaW50byBWZWNoZQAAAAAAAAAAAAAAAAAA"
