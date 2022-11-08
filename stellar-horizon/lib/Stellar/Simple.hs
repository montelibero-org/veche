{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Stellar.Simple (
    Address (..),
    Asset (..),
    mkAsset,
    xlm,
    Memo (..),
    Operation (..),
    Transaction (..),
    TransactionOnChain (..),
    TxId,
    -- * Transaction builder
    transactionBuilder,
    addPayment,
    addSetHomeDomain,
    addManageData,
    setMemoText,
    build,
    signWithSecret,
    xdrSerializeBase64T,
) where

-- prelude
import Prelude hiding (id)
import Prelude qualified

-- global
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import GHC.Stack (HasCallStack)
import Network.HTTP.Client.TLS (getGlobalManager)
import Servant.Client (ClientM, mkClientEnv, runClientM)
import Text.Read (readEither)

-- stellar-sdk
import Network.ONCRPC.XDR (XDR, xdrSerialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Builder qualified as XdrBuilder
import Network.Stellar.Keypair qualified as StellarKey
import Network.Stellar.Network (publicNetwork)
import Network.Stellar.TransactionXdr qualified as XDR

-- component
import Stellar.Horizon.Client (decodeUtf8Throw, getAccount, publicServerBase,
                               xlm)
import Stellar.Horizon.DTO (Address (Address), TxId)
import Stellar.Horizon.DTO qualified as DTO
import Stellar.Simple.Types (Asset (..), Memo (..), Operation (..),
                             Transaction (..), TransactionOnChain (..))

identity :: a -> a
identity = Prelude.id

-- | Make asset from the canonical pair of code and issuer
mkAsset :: Text -> Text -> Asset
mkAsset code issuer = Asset{code, issuer = Just issuer}

data TransactionBuilder = TransactionBuilder
    { account       :: Address
    , memo          :: Memo
    , operations    :: Seq XDR.Operation
    }

transactionBuilder :: Address -> TransactionBuilder
transactionBuilder account =
    TransactionBuilder{account, memo = MemoNone, operations = mempty}

setMemoText :: Text -> TransactionBuilder -> TransactionBuilder
setMemoText t b = b{memo = MemoText t}

build :: HasCallStack => TransactionBuilder -> IO XDR.TransactionEnvelope
build TransactionBuilder{account, memo, operations} = do
    DTO.Account{sequence = sequenceText} <- runClientPublic $ getAccount account
    let seqNum = either error identity $ readEither $ Text.unpack sequenceText
        tx =
            XDR.Transaction
            { transaction'cond
            , transaction'fee
            , transaction'memo
            , transaction'operations
            , transaction'seqNum = seqNum + 1
            , transaction'sourceAccount
            , transaction'v = 0
            }
    pure $
        XDR.TransactionEnvelope'ENVELOPE_TYPE_TX $
        XDR.TransactionV1Envelope tx XDR.emptyBoundedLengthArray
  where
    transaction'cond = XDR.Preconditions'PRECOND_NONE
    transaction'fee = defaultFee * fromIntegral (length operations)
    transaction'memo =
        case memo of
            MemoNone    -> XDR.Memo'MEMO_NONE
            MemoText t  -> XDR.Memo'MEMO_TEXT $ XDR.lengthArray' $ encodeUtf8 t
            MemoOther{} -> undefined
    transaction'operations = XDR.boundLengthArrayFromList $ toList operations
    transaction'sourceAccount = addressToXdrMuxed account

addressToXdr :: Address -> XDR.AccountID
addressToXdr (Address address) =
    XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 $
    XDR.lengthArray' $ StellarKey.decodePublic' address

addressToXdrMuxed :: Address -> XDR.MuxedAccount
addressToXdrMuxed (Address address) =
    XDR.MuxedAccount'KEY_TYPE_ED25519 $
    XDR.lengthArray' $ StellarKey.decodePublic' address

defaultFee :: Word32
defaultFee = 100

runClientPublic :: HasCallStack => ClientM a -> IO a
runClientPublic action = do
    manager <- getGlobalManager
    let env = mkClientEnv manager publicServerBase
    e <- runClientM action env
    either (error . show) pure e

addPayment ::
    Address -> Asset -> Int64 -> TransactionBuilder -> TransactionBuilder
addPayment address asset amount b@TransactionBuilder{operations} =
    b{operations = operations |> op}
  where
    op = XDR.Operation{operation'sourceAccount = Nothing, operation'body}
    operation'body = XDR.OperationBody'PAYMENT payment
    payment =
        XDR.PaymentOp
        { paymentOp'destination = addressToXdrMuxed address
        , paymentOp'asset = assetToXdr asset
        , paymentOp'amount = amount
        }

addManageData :: Text -> Maybe Text -> TransactionBuilder -> TransactionBuilder
addManageData key mvalue b@TransactionBuilder{operations} =
    b{operations = operations |> op}
  where
    op = XDR.Operation{operation'sourceAccount = Nothing, operation'body}
    operation'body = XDR.OperationBody'MANAGE_DATA data_
    data_ =
        XDR.ManageDataOp
            (XDR.lengthArray' $ encodeUtf8 key)
            (XDR.lengthArray' . encodeUtf8 <$> mvalue)

assetToXdr :: Asset -> XDR.Asset
assetToXdr = \case
    Asset{issuer = Nothing} -> XDR.Asset'ASSET_TYPE_NATIVE
    Asset{code, issuer = Just issuer}
        | Text.length code <= 4 ->
            XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM4 $
            XDR.AlphaNum4
                (XDR.lengthArray' $ encodeUtf8 code)
                (addressToXdr $ Address issuer)
        | otherwise ->
            XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM12 $
            XDR.AlphaNum12
                (XDR.lengthArray' $ encodeUtf8 code)
                (addressToXdr $ Address issuer)

signWithSecret ::
    HasCallStack =>
    -- | "S..." textual secret key
    Text ->
    XDR.TransactionEnvelope ->
    XDR.TransactionEnvelope
signWithSecret secret tx =
    either (error . show) identity $
    XdrBuilder.sign publicNetwork tx [StellarKey.fromPrivateKey' secret]

xdrSerializeBase64T :: XDR a => a -> Text
xdrSerializeBase64T = decodeUtf8Throw . Base64.encode . xdrSerialize

defaultOptions :: XDR.SetOptionsOp
defaultOptions =
    XDR.SetOptionsOp
        { setOptionsOp'inflationDest    = Nothing
        , setOptionsOp'clearFlags       = Nothing
        , setOptionsOp'setFlags         = Nothing
        , setOptionsOp'masterWeight     = Nothing
        , setOptionsOp'lowThreshold     = Nothing
        , setOptionsOp'medThreshold     = Nothing
        , setOptionsOp'highThreshold    = Nothing
        , setOptionsOp'homeDomain       = Nothing
        , setOptionsOp'signer           = Nothing
        }

addSetHomeDomain :: Text -> TransactionBuilder -> TransactionBuilder
addSetHomeDomain domain b@TransactionBuilder{operations} =
    b{operations = operations |> op}
  where
    op = XDR.Operation{operation'sourceAccount = Nothing, operation'body}
    operation'body = XDR.OperationBody'SET_OPTIONS options
    options = defaultOptions{XDR.setOptionsOp'homeDomain}
    setOptionsOp'homeDomain = Just $ XDR.lengthArray' $ encodeUtf8 domain
