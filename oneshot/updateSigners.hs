import Data.Foldable
import Data.Function as Function
import Data.Functor
import Data.Map.Strict (Map, (\\))
import Data.Map.Strict qualified as Map
import Data.Scientific
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
    let holders' :: Map Address Scientific =
            Map.fromList
                [ (account_id, amount)
                | Account{account_id, balances} <- holders
                , let amount = getAssetAmount token balances
                ]
    let newSigners :: Map Address Word8 =
            holders' <&> \a -> round $ a * 255 / sum holders'
    pPrint holders'

    for_ accounts \account -> do
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
            client
            & transactionBuilder account
            & op_setSigners signersDiff
            & build
        secret <- Text.strip <$> Text.readFile "/tmp/secret"
        let envelope = signWithSecret secret tx
        Text.putStrLn $ envelope & xdrSerializeBase64T
  where
    distributor = "GD5TGAX3IM2EFA4PBTDNHAM2U5UPAV6DYVR6TA2KCONFFSUOID3ZB6HA"
    issuer = "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
    token = mkAsset "VECHE" issuer
    accounts = [Address distributor, Address issuer]

getAssetAmount :: Asset -> [Balance] -> Scientific
getAssetAmount Asset{code, issuer} balances =
    sum [ either error Function.id $ readEither $ Text.unpack balance
        | Balance{asset_code, asset_issuer, balance} <- balances
        , asset_code == Just code
        , asset_issuer == issuer
        ]
