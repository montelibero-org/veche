import Data.Function as Function
import Data.Map.Strict (Map, (\\))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Word
import Named
import Text.Pretty.Simple
import Text.Read

import Stellar.Horizon.Client
import Stellar.Horizon.DTO
import Stellar.Simple

main :: IO ()
main = do
    client <- getPublicClient ! defaults

    holders <- runClientThrow (getAccountsList token) client
    pPrint holders
    let holders' :: Map Address Double =
            Map.fromList
                [ (account_id, amount)
                | Account{account_id, balances} <- holders
                , let amount = getAssetAmount token balances
                ]
    pPrint holders'
    let newSigners :: Map Address Word8 =
            holders'
            & fmap (\a -> round $ a * 255 / sum holders')
            & Map.filter (/= 0)
    pPrint newSigners

    let threshold = (fromIntegral (sum newSigners) + 2) `div` 2
        -- ^ minimum that greater than n/2
    pPrint threshold

    oldSigners :: Map Address Word8 <- do
        Account{signers} <- runClientThrow (getAccount account) client
        pure $
            Map.fromList
                [ (Address key, fromIntegral weight)
                | Signer{type_ = Ed25519PublicKey, key, weight} <- signers
                ]
    pPrint oldSigners

    let signersDiff :: Map Address Word8 =
            newSigners <> (0 <$ oldSigners \\ newSigners)
    pPrint signersDiff

    tx <-
        transactionBuilder account
        & tx_feePerOp_guess
        & op_setSigners signersDiff
        & op_setThresholds (#low threshold) (#med threshold) (#high threshold)
        & build client
    secret <- Text.strip <$> Text.readFile "/tmp/secret"
    let envelope = signWithSecret secret tx
    Text.putStrLn $ envelope & xdrSerializeBase64T

  where
    issuer = "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
    token = mkAsset "VECHE" issuer
    account = Address issuer

getAssetAmount :: Asset -> [Balance] -> Double
getAssetAmount Asset{code, issuer} balances =
    sum [ either error Function.id $ readEither $ Text.unpack balance
        | Balance{asset_code, asset_issuer, balance} <- balances
        , asset_code == Just code
        , asset_issuer == issuer
        ]
