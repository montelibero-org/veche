{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    op_setMasterWeight,
    op_setSigners,
    op_setThresholds,
    op_manageData,
    op_manageSellOffer,
    tx_feePerOp,
    tx_feePerOp_guess,
    tx_memoText,
    tx_seqNum,
    build,
    signWithSecret,
    xdrSerializeBase64T,
    -- * Client
    runClientThrow,
    getPublicClient,
    submit,
    retryOnTimeout,
    -- * Verification
    verifyTx,
) where

-- prelude
import Prelude hiding (id)
import Prelude qualified

-- global
import Control.Exception (SomeException (SomeException), catchJust)
import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (toList)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (Endo), appEndo)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (cast)
import Data.Word (Word32, Word8)
import GHC.Stack (HasCallStack)
import Named (NamedF (Arg, ArgF), (:!), (:?))
import Network.HTTP.Client (HttpException (HttpExceptionRequest),
                            HttpExceptionContent (ResponseTimeout),
                            ResponseTimeout, managerResponseTimeout,
                            responseTimeoutDefault)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (gatewayTimeout504)
import Servant.Client (ClientEnv,
                       ClientError (ConnectionError, FailureResponse), ClientM,
                       ResponseF (Response), mkClientEnv, runClientM)
import Servant.Client qualified
import Text.Read (readEither, readMaybe)

-- stellar-sdk
import Network.ONCRPC.XDR (XDR, xdrSerialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Keypair qualified as StellarKey
import Network.Stellar.Network (Network, publicNetwork)
import Network.Stellar.Signature qualified as StellarSignature
import Network.Stellar.TransactionXdr (Uint256)
import Network.Stellar.TransactionXdr qualified as XDR

-- project
import WithCallStack (WithCallStack (WithCallStack), throwWithCallStackIO)
import WithCallStack qualified

-- component
import Stellar.Horizon.API (TxText (TxText))
import Stellar.Horizon.Client (decodeUtf8Throw, getAccount, getFeeStats,
                               publicServerBase, submitTransaction, xlm)
import Stellar.Horizon.DTO (Address (Address), FeeStats (FeeStats), TxId)
import Stellar.Horizon.DTO qualified as DTO
import Stellar.Simple.Types (Asset (..), DecoratedSignature (..), Memo (..),
                             Operation (..), Transaction (..),
                             TransactionOnChain (..))

identity :: a -> a
identity = Prelude.id

-- | Make asset from the canonical pair of code and issuer
mkAsset :: Text -> Text -> Asset
mkAsset code issuer = Asset{code, issuer = Just issuer}

data Guess a = Already a | Guess
    deriving (Show)

data TransactionBuilder = TransactionBuilder
    { account       :: Address
    , feePerOp      :: Guess Word32
    , memo          :: Memo
    , operations    :: Seq XDR.Operation
    , seqNum        :: Guess Int64
    }
    deriving (Show)

getPublicClient :: "responseTimeout" :? ResponseTimeout -> IO ClientEnv
getPublicClient (ArgF responseTimeout) = do
    manager <-
        newTlsManagerWith
            tlsManagerSettings
            { managerResponseTimeout =
                fromMaybe responseTimeoutDefault responseTimeout
            }
    pure $ mkClientEnv manager publicServerBase

transactionBuilder :: Address -> TransactionBuilder
transactionBuilder account =
    TransactionBuilder
    { account
    , feePerOp = Already defaultFeePerOp
    , memo = MemoNone
    , operations = mempty
    , seqNum = Guess
    }

tx_feePerOp_guess :: TransactionBuilder -> TransactionBuilder
tx_feePerOp_guess b = b{feePerOp = Guess}

tx_feePerOp :: Word32 -> TransactionBuilder -> TransactionBuilder
tx_feePerOp feePerOp b = b{feePerOp = Already feePerOp}

tx_memoText :: Text -> TransactionBuilder -> TransactionBuilder
tx_memoText t TransactionBuilder{..} = TransactionBuilder{memo = MemoText t, ..}

tx_seqNum :: Int64 -> TransactionBuilder -> TransactionBuilder
tx_seqNum n b = b{seqNum = Already n}

build ::
    HasCallStack =>
    ClientEnv -> TransactionBuilder -> IO XDR.TransactionEnvelope
build   clientEnv
        TransactionBuilder{account, feePerOp, memo, operations, seqNum}
        = do
    feePerOp' <-
        case feePerOp of
            Already f   -> pure f
            Guess       -> guessFee
    transaction'seqNum <-
        case seqNum of
            Already n   -> pure n
            Guess       -> succ <$> fetchSeqNum
    let transaction'fee = feePerOp' * fromIntegral (length operations)
        tx =
            XDR.Transaction
            { transaction'cond
            , transaction'fee
            , transaction'memo
            , transaction'operations
            , transaction'seqNum -- = seqNum + 1
            , transaction'sourceAccount
            , transaction'v = 0
            }
    pure $
        XDR.TransactionEnvelope'ENVELOPE_TYPE_TX $
        XDR.TransactionV1Envelope tx XDR.emptyBoundedLengthArray
  where

    transaction'cond = XDR.Preconditions'PRECOND_NONE

    transaction'memo =
        case memo of
            MemoNone    -> XDR.Memo'MEMO_NONE
            MemoText t  -> XDR.Memo'MEMO_TEXT $ XDR.lengthArray' $ encodeUtf8 t
            MemoOther{} -> undefined

    transaction'operations = XDR.boundLengthArrayFromList $ toList operations

    transaction'sourceAccount =
        XDR.MuxedAccount'KEY_TYPE_ED25519 $ addressToXdr account

    guessFee = do
        FeeStats{fee_charged} <- runClientThrow getFeeStats clientEnv
        pure $
            fromMaybe defaultFeePerOp do
                t <- Map.lookup "min" fee_charged
                readMaybe $ Text.unpack t

    fetchSeqNum = do
        DTO.Account{sequence = sequenceText} <-
            runClientThrow (getAccount account) clientEnv
        either fail pure $ readEither $ Text.unpack sequenceText

addressToXdr :: Address -> Uint256
addressToXdr (Address address) =
    -- XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 $
    XDR.lengthArray' $ StellarKey.decodePublic' address

defaultFeePerOp :: Word32
defaultFeePerOp = 100

runClientThrow :: HasCallStack => ClientM a -> ClientEnv -> IO a
runClientThrow action env = do
    e <- runClientM action env
    either throwWithCallStackIO pure e

op_payment ::
    Asset ->
    Int64 ->
    "destination" :! Address ->
    "source" :? Address ->
    TransactionBuilder ->
    TransactionBuilder
op_payment asset amount (Arg destination) (ArgF source) =
    addOperation source $
    XDR.OperationBody'PAYMENT $
    XDR.PaymentOp
    { paymentOp'destination =
        XDR.MuxedAccount'KEY_TYPE_ED25519 $ addressToXdr destination
    , paymentOp'asset = assetToXdr asset
    , paymentOp'amount = amount
    }

op_manageData :: Text -> Maybe Text -> TransactionBuilder -> TransactionBuilder
op_manageData key mvalue =
    addOperation Nothing $
    XDR.OperationBody'MANAGE_DATA $
    XDR.ManageDataOp
        (XDR.lengthArray' $ encodeUtf8 key)
        (XDR.lengthArray' . encodeUtf8 <$> mvalue)

addOperation ::
    Maybe Address ->
    XDR.OperationBody ->
    TransactionBuilder ->
    TransactionBuilder
addOperation msource operation'body TransactionBuilder{..} =
    TransactionBuilder
    { operations =
        operations
        |> XDR.Operation
            { operation'sourceAccount =
                XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 . addressToXdr <$> msource
            , operation'body
            }
    , ..
    }

type Price = Ratio Int32

priceToXdr :: Ratio Int32 -> XDR.Price
priceToXdr r = XDR.Price{price'n = numerator r, price'd = denominator r}

op_manageSellOffer ::
    "selling"   :! Asset    ->
    "unit"      :! Asset    ->
    "amount"    :! Int64    ->
    "price"     :! Price    ->
    "source"    :? Address  ->
    TransactionBuilder          ->
    TransactionBuilder
op_manageSellOffer
        (Arg selling) (Arg unit) (Arg amount) (Arg price) (ArgF source) =
    addOperation source $
    XDR.OperationBody'MANAGE_SELL_OFFER $
    XDR.ManageSellOfferOp
        { manageSellOfferOp'amount  = amount
        , manageSellOfferOp'buying  = assetToXdr unit
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
                (   XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 $
                    addressToXdr $ Address issuer
                )
        | otherwise ->
            XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM12 $
            XDR.AlphaNum12
                (XDR.padLengthArray (encodeUtf8 code) 0)
                (   XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 $
                    addressToXdr $ Address issuer
                )

signWithSecret ::
    HasCallStack =>
    -- | "S..." textual secret key
    Text ->
    XDR.TransactionEnvelope ->
    XDR.TransactionEnvelope
signWithSecret secret tx =
    either (error . show) identity $
    StellarSignature.signTx publicNetwork tx [StellarKey.fromPrivateKey' secret]

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
    addOperation Nothing $
    XDR.OperationBody'SET_OPTIONS
        defaultOptions
        { XDR.setOptionsOp'homeDomain =
            Just $ XDR.lengthArray' $ encodeUtf8 domain
        }

op_setMasterWeight :: Word32 -> TransactionBuilder -> TransactionBuilder
op_setMasterWeight w =
    addOperation Nothing $
    XDR.OperationBody'SET_OPTIONS
    defaultOptions{XDR.setOptionsOp'masterWeight = Just w}

op_setSigners :: Map Address Word8 -> TransactionBuilder -> TransactionBuilder
op_setSigners =
    appEndo
    . Map.foldMapWithKey \address weight ->
        Endo $
        addOperation Nothing $
        XDR.OperationBody'SET_OPTIONS
            defaultOptions
            { XDR.setOptionsOp'signer =
                Just
                XDR.Signer
                { signer'key =
                    XDR.SignerKey'SIGNER_KEY_TYPE_ED25519 $ addressToXdr address
                , signer'weight = fromIntegral weight
                }
            }

op_setThresholds ::
    "low"  :? Word32 ->
    "med"  :? Word32 ->
    "high" :? Word32 ->
    TransactionBuilder ->
    TransactionBuilder
op_setThresholds (ArgF low) (ArgF med) (ArgF high) =
    addOperation Nothing $
    XDR.OperationBody'SET_OPTIONS $
    defaultOptions
    { XDR.setOptionsOp'lowThreshold  = low
    , XDR.setOptionsOp'medThreshold  = med
    , XDR.setOptionsOp'highThreshold = high
    }

submit :: XDR.TransactionEnvelope -> ClientEnv -> IO DTO.Transaction
submit = runClientThrow . submitTransaction . TxText . xdrSerializeBase64T

retryOnTimeout :: IO a -> IO a
retryOnTimeout action =
    catchJust guardTimeout action $ const $ retryOnTimeout action
  where
    guardTimeout = \case
        e1@WithCallStack{parent = SomeException e2}
            | Just (ConnectionError (SomeException e3)) <- cast e2
            , Just (HttpExceptionRequest _req ResponseTimeout) <- cast e3
            ->
                Just e1
        e1@WithCallStack{parent = SomeException e2}
            | Just (FailureResponse _req Response{responseStatusCode}) <-
                cast e2
            , responseStatusCode == gatewayTimeout504
            ->
                Just e1
        _ -> Nothing

verifyTx
    :: Network
    -> XDR.TransactionEnvelope
    -> Address
    -> DecoratedSignature
    -> Bool
verifyTx net envelope (Address publicKey) (DecoratedSignature _ signature) =
    Ed25519.dverify
        (StellarKey.decodePublicKey' publicKey)
        (StellarSignature.transactionHash net envelope)
        (Ed25519.Signature signature)
