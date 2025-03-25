{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

import Control.Exception (SomeException (SomeException), try)
import Data.Function ((&))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (cast)
import Named (defaults, (!))
import Servant.Client (ClientError (FailureResponse))
import Stellar.Horizon.DTO (Transaction)
import Stellar.Simple (
    Address (Address),
    Asset,
    build,
    getPublicClient,
    mkAsset,
    op_accountMerge,
    op_deleteTrust,
    op_payment,
    signWithSecret,
    submit,
    transactionBuilder,
    tx_feePerOp_guess,
    xdrSerializeBase64T,
 )
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import Text.Pretty.Simple (pHPrint, pPrint)
import WithCallStack (WithCallStack (WithCallStack))
import WithCallStack qualified

main :: IO ()
main = do
    client <- getPublicClient ! defaults
    t <-
        transactionBuilder escrowAddress
            & tx_feePerOp_guess
            & op_payment eurmtl 2497870 ! #destination destination ! defaults
            & op_deleteTrust ! #line eurmtl
            & op_accountMerge ! #destination destination
            & build client
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret t
    Text.putStrLn $ xdrSerializeBase64T envelope
    submitResult <- try $ submit envelope client
    printSubmitResult submitResult
  where
    destination =
        Address "GCPT3X4FJBMUBR5AIB7SEUQX7HJ4XX3K4TNI2J7WIHMHMFGDMRRJJVWL"

printSubmitResult :: Either SomeException Transaction -> IO ()
printSubmitResult = \case
    Right res -> pPrint res
    Left someException -> do
        unwrap someException
        exitFailure
  where
    unwrap (SomeException se)
        | Just WithCallStack{parent} <- cast se = unwrap parent
        | Just (FailureResponse _ response) <- cast se = pEPrint response
        | otherwise = ePrint se

ePrint :: (Show a) => a -> IO ()
ePrint = hPrint stderr

pEPrint :: (Show a) => a -> IO ()
pEPrint = pHPrint stderr

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
