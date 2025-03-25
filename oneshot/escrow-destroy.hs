{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

import Data.Function ((&))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Named (defaults, (!))
import Stellar.Simple (
    Address (Address),
    Asset,
    build,
    getPublicClient,
    mkAsset,
    op_payment,
    signWithSecret,
    submit,
    transactionBuilder,
    tx_feePerOp_guess,
    tx_memoText,
    xdrSerializeBase64T,
 )
import Text.Pretty.Simple (pPrint)

--  18  40.1
--  20  10.0
--  25  10.0
--  26  50.0

-- main :: IO ()
-- main = do
--     [escrowFile] <- getArgs
--     escrowE <- eitherDecodeFileStrict' @[TransactionOnChain] escrowFile
--     escrow <- either fail pure escrowE
--     for_ escrow \toc ->
--         withCallContext ("toc = " <> TextL.unpack (pShow toc)) do
--             let tx = toc.tx
--             for_ tx.operations \opE -> do
--                 Right op@OperationPayment{} <- pure opE
--                 op.asset === eurmtl
--                 let (amount, donor)
--                         | op.destination == escrowAddress =
--                             (op.amount, fromMaybe tx.source op.source)
--                         | (tx.source, op.source)
--                             == (escrowAddress, Nothing) =
--                             (-op.amount, op.destination)
--                         | otherwise = undefined
--                 let issue = correct toc.id $ memoToText tx.memo
--                 when (issue `elem` ["18", "20", "25", "26"]) $
--                     pPrint (issue, donor, amount)

main :: IO ()
main = do
    client <- getPublicClient ! defaults
    t <-
        transactionBuilder escrowAddress
            & tx_feePerOp_guess
            & tx_memoText ("E" <> issue)
            & op_payment eurmtl amount
                ! #destination destination
                ! defaults
            & build client
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret t
    Text.putStrLn $ xdrSerializeBase64T envelope
    submit envelope client >>= pPrint
  where
    issue = "26"
    destination =
        Address ""
    amount = 50e7

eurmtl :: Asset
eurmtl =
    mkAsset "EURMTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"

escrowAddress :: Address
escrowAddress =
    Address "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"

-- (===) :: (HasCallStack, Eq a, Show a, Applicative f) => a -> a -> f ()
-- x === y = assert (unwords [show x, "/=", show y]) $ x == y

-- assert :: (HasCallStack, Applicative f) => String -> Bool -> f ()
-- assert message condition = unless condition $ error message

-- withCallContext :: (HasCallStack) => String -> ((HasCallStack) => a) -> a
-- withCallContext context do_this =
--     let ?callStack = pushCallStack (context, loc) $ popCallStack callStack
--      in do_this
--   where
--     (_, loc) = head $ getCallStack callStack

-- memoToText :: Memo -> Maybe String
-- memoToText = \case
--     MemoNone -> Nothing
--     MemoText t -> Just $ Text.unpack t
--     MemoOther _ -> undefined

-- correct :: (HasCallStack) => TxId -> Maybe String -> String
-- correct
--     (TxId "d673b5f0697686bf764bca37f8357ded645b62e63e7e78b5c40fa15e506015e6")
--     _ =
--         "EXCLUDE"
-- correct
--     (TxId "fd420c7803da760e6d0bc4cb4bde1fa42287c4f20ef6d70ba607671aff403f05")
--     _ =
--         "EXCLUDE"
-- correct
--     (TxId "2e4f2bda359022d4a6184da8f1a595f22d07df0d331c05d1eb4743b142ed6ec3")
--     _ =
--         "17"
-- correct
--     (TxId "a74ea166cc02214345e23ab86c6d45abf37efba3f1827759459b076952db1b4c")
--     _ =
--         "25"
-- correct _ (Just memo)
--     | 'E' : eid <- memo = eid
--     | "Key Rate " `isPrefixOf` memo = "KEYRATE"
-- correct txid memo = error $ show (txid, memo)
