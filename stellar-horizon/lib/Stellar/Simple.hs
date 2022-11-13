{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Stellar.Simple (
    Address (..),
    Asset (..),
    mkAsset,
    xlm,
    Memo (..),
    Operation (..),
    Transaction (..),
    TransactionOnChain (..),
    TxId (..),
    -- * Transaction builder
    transactionBuilder,
    op_payment,
    op_setHomeDomain,
    op_manageData,
    op_manageSellOffer,
    tx_feePerOp,
    tx_memoText,
    build,
    signWithSecret,
    xdrSerializeBase64T,
    -- * Client
    getPublicClient,
    submit,
) where

-- prelude
import Prelude hiding (id)
import Prelude qualified

-- global
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (toList)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import GHC.Stack (HasCallStack)
import Named (NamedF (Arg, ArgF), (:!), (:?))
import Network.HTTP.Client (ResponseTimeout, managerResponseTimeout,
                            responseTimeoutDefault)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)
import Text.Read (readEither)

-- stellar-sdk
import Network.ONCRPC.XDR (XDR, xdrSerialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Builder qualified as XdrBuilder
import Network.Stellar.Keypair qualified as StellarKey
import Network.Stellar.Network (publicNetwork)
import Network.Stellar.TransactionXdr qualified as XDR

-- project
import WithCallStack (throwWithCallStackIO)

-- component
import Stellar.Horizon.API (TxText (TxText))
import Stellar.Horizon.Client (decodeUtf8Throw, getAccount, publicServerBase,
                               submitTransaction, xlm)
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
    , clientEnv     :: ClientEnv
    , feePerOp      :: Word32
    , memo          :: Memo
    , operations    :: Seq XDR.Operation
    }

getPublicClient :: "responseTimeout" :? ResponseTimeout -> IO ClientEnv
getPublicClient (ArgF responseTimeout) = do
    manager <-
        newTlsManagerWith
            tlsManagerSettings
            { managerResponseTimeout =
                fromMaybe responseTimeoutDefault responseTimeout
            }
    pure $ mkClientEnv manager publicServerBase

transactionBuilder :: Address -> ClientEnv -> TransactionBuilder
transactionBuilder account clientEnv =
    TransactionBuilder
    { account
    , clientEnv
    , feePerOp = defaultFeePerOp
    , memo = MemoNone
    , operations = mempty
    }

tx_feePerOp :: Word32 -> TransactionBuilder -> TransactionBuilder
tx_feePerOp feePerOp b = b{feePerOp}

tx_memoText :: Text -> TransactionBuilder -> TransactionBuilder
tx_memoText t b = b{memo = MemoText t}

build :: HasCallStack => TransactionBuilder -> IO XDR.TransactionEnvelope
build TransactionBuilder{account, clientEnv, feePerOp, memo, operations} = do
    DTO.Account{sequence = sequenceText} <-
        runClientThrow (getAccount account) clientEnv
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
    transaction'fee = feePerOp * fromIntegral (length operations)
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

defaultFeePerOp :: Word32
defaultFeePerOp = 100

runClientThrow :: HasCallStack => ClientM a -> ClientEnv -> IO a
runClientThrow action env = do
    -- manager <- getGlobalManager
    -- let env = mkClientEnv manager publicServerBase
    e <- runClientM action env
    either throwWithCallStackIO pure e

op_payment ::
    Address -> Asset -> Int64 -> TransactionBuilder -> TransactionBuilder
op_payment address asset amount =
    addOperation $
    XDR.OperationBody'PAYMENT $
    XDR.PaymentOp
    { paymentOp'destination = addressToXdrMuxed address
    , paymentOp'asset = assetToXdr asset
    , paymentOp'amount = amount
    }

op_manageData :: Text -> Maybe Text -> TransactionBuilder -> TransactionBuilder
op_manageData key mvalue =
    addOperation $
    XDR.OperationBody'MANAGE_DATA $
    XDR.ManageDataOp
        (XDR.lengthArray' $ encodeUtf8 key)
        (XDR.lengthArray' . encodeUtf8 <$> mvalue)

addOperation :: XDR.OperationBody -> TransactionBuilder -> TransactionBuilder
addOperation operation'body b@TransactionBuilder{operations} =
    b   { operations =
            operations
            |> XDR.Operation{operation'sourceAccount = Nothing, operation'body}
        }

type Price = Ratio Int32

priceToXdr :: Ratio Int32 -> XDR.Price
priceToXdr r = XDR.Price{price'n = numerator r, price'd = denominator r}

op_manageSellOffer ::
    "selling" :! Asset ->
    "buying"  :! Asset ->
    Int64 ->
    Price ->
    TransactionBuilder ->
    TransactionBuilder
op_manageSellOffer (Arg selling) (Arg buying) amount price =
    addOperation $
    XDR.OperationBody'MANAGE_SELL_OFFER $
    XDR.ManageSellOfferOp
        { manageSellOfferOp'amount  = amount
        , manageSellOfferOp'buying  = assetToXdr buying
        , manageSellOfferOp'offerID = 0
        , manageSellOfferOp'price   = priceToXdr price
        , manageSellOfferOp'selling = assetToXdr selling
        }

assetToXdr :: Asset -> XDR.Asset
assetToXdr = \case
    Asset{issuer = Nothing} -> XDR.Asset'ASSET_TYPE_NATIVE
    Asset{code, issuer = Just issuer}
        | Text.length code <= 4 ->
            XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM4 $
            XDR.AlphaNum4
                (XDR.padLengthArray (encodeUtf8 code) 0)
                (addressToXdr $ Address issuer)
        | otherwise ->
            XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM12 $
            XDR.AlphaNum12
                (XDR.padLengthArray (encodeUtf8 code) 0)
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

op_setHomeDomain :: Text -> TransactionBuilder -> TransactionBuilder
op_setHomeDomain domain =
    addOperation $
    XDR.OperationBody'SET_OPTIONS $
    defaultOptions
    {XDR.setOptionsOp'homeDomain = Just $ XDR.lengthArray' $ encodeUtf8 domain}

submit :: XDR.TransactionEnvelope -> ClientEnv -> IO DTO.Transaction
submit = runClientThrow . submitTransaction . TxText . xdrSerializeBase64T
