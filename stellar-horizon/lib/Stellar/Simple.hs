{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Stellar.Simple (
    Address (..),
    Asset (..),
    assetFromText,
    assetToText,
    mkAsset,
    xlm,
    Memo (..),
    Operation (..),
    Transaction (..),
    transactionFromDto,
    transactionFromEnvelopeXdr,
    TxId,
) where

import Prelude hiding (id)
import Prelude qualified

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.Stack (HasCallStack)
import Network.ONCRPC.XDR (xdrDeserialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Keypair qualified as StellarKey
import Network.Stellar.TransactionXdr (DecoratedSignature)
import Network.Stellar.TransactionXdr qualified as XDR
import Servant.API (FromHttpApiData, ToHttpApiData)
import Servant.API qualified

import Stellar.Horizon.DTO (Address (Address), TxId)
import Stellar.Horizon.DTO qualified as DTO

identity :: a -> a
identity = Prelude.id

addressFromXdr :: XDR.AccountID -> Address
addressFromXdr (XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 address) =
    Address $ StellarKey.encodePublic $ XDR.unLengthArray address

addressFromXdrMuxed :: XDR.MuxedAccount -> Address
addressFromXdrMuxed =
    Address
    . StellarKey.encodePublic
    . XDR.unLengthArray
    . \case
        XDR.MuxedAccount'KEY_TYPE_ED25519 address -> address
        XDR.MuxedAccount'KEY_TYPE_MUXED_ED25519 _id address -> address

amountFromXdr :: Int64 -> Scientific
amountFromXdr i = scientific (fromIntegral i) (-7)

data Asset = Asset{issuer :: Maybe Text, code :: Text}
    deriving (Eq, Ord)

-- | Make asset from the canonical pair of code and issuer
mkAsset :: Text -> Text -> Asset
mkAsset code issuer = Asset{code, issuer = Just issuer}

-- | Native asset
xlm :: Asset
xlm = Asset{code = "XLM", issuer = Nothing}

-- | Representation is "XLM" or "{code}:{issuer}"
assetToText :: Asset -> Text
assetToText Asset{code, issuer} = code <> maybe "" (":" <>) issuer

assetFromText :: Text -> Asset
assetFromText t
    | Text.null _issuer = Asset{code = t, issuer = Nothing}
    | otherwise         = Asset{code, issuer = Just issuer}
  where
    (code, _issuer) = Text.break (== ':') t
    issuer = Text.drop 1 _issuer

instance Show Asset where
    show = Text.unpack . assetToText

instance FromHttpApiData Asset where
    parseUrlPiece = Right . assetFromText

instance ToHttpApiData Asset where
    toUrlPiece = assetToText

instance FromJSON Asset where
    parseJSON = Aeson.withText "Asset" $ pure . assetFromText

instance ToJSON Asset where
    toJSON = toJSON . assetToText

assetFromXdr :: XDR.Asset -> Asset
assetFromXdr = \case
    XDR.Asset'ASSET_TYPE_NATIVE -> xlm
    XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM4 (XDR.AlphaNum4 code issuer) ->
        assetAlphaNum code issuer
    XDR.Asset'ASSET_TYPE_CREDIT_ALPHANUM12 (XDR.AlphaNum12 code issuer) ->
        assetAlphaNum code issuer
  where
    assetAlphaNum code (XDR.PublicKey'PUBLIC_KEY_TYPE_ED25519 issuer) =
        Asset
            { code =
                decodeUtf8Throw $
                BS.dropWhileEnd (== 0) $ XDR.unLengthArray code
            , issuer = Just $ StellarKey.encodePublic $ XDR.unLengthArray issuer
            }

data Memo = MemoNone | MemoText Text | MemoOther XDR.Memo
    deriving (Eq)

memoFromXdr :: XDR.Memo -> Memo
memoFromXdr = \case
    XDR.Memo'MEMO_NONE -> MemoNone
    XDR.Memo'MEMO_TEXT text ->
        MemoText $ decodeUtf8Throw $ XDR.unLengthArray text
    memo -> MemoOther memo

data Operation
    = OperationManageData ByteString (Maybe ByteString)
    | OperationPayment
        { asset         :: Asset
        , amount        :: Scientific
        , destination   :: Address
        , source        :: Maybe Address
        }
    | OperationOther XDR.Operation

operationFromXdr :: XDR.Operation -> Operation
operationFromXdr op@XDR.Operation{operation'body, operation'sourceAccount} =
    case operation'body of
        XDR.OperationBody'MANAGE_DATA (XDR.ManageDataOp name value) ->
            OperationManageData
                (XDR.unLengthArray name) (XDR.unLengthArray <$> value)
        XDR.OperationBody'PAYMENT
                XDR.PaymentOp
                    { paymentOp'amount
                    , paymentOp'asset
                    , paymentOp'destination
                    } ->
            OperationPayment
                { asset         = assetFromXdr          paymentOp'asset
                , amount        = amountFromXdr         paymentOp'amount
                , destination   = addressFromXdrMuxed   paymentOp'destination
                , source        = addressFromXdr <$>    operation'sourceAccount
                }
        _ -> OperationOther op

data Transaction = Transaction
    { id            :: TxId
    , memo          :: Memo
    , operations    :: [Operation]
    , signatures    :: [DecoratedSignature]
    , source        :: Address
    }

transactionFromDto :: HasCallStack => DTO.Transaction -> Transaction
transactionFromDto DTO.Transaction{id, envelope_xdr} =
    transactionFromEnvelopeXdr id envelope
  where
    envelopeXdrRaw =
        either error identity $ Base64.decode $ encodeUtf8 envelope_xdr
    envelope = either error identity $ xdrDeserialize envelopeXdrRaw

transactionFromEnvelopeXdr :: TxId -> XDR.TransactionEnvelope -> Transaction
transactionFromEnvelopeXdr id = \case
    XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_V0       e -> fromV0 e
    XDR.TransactionEnvelope'ENVELOPE_TYPE_TX          e -> fromV1 e
    XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP e -> fromFB e
  where

    fromV0  (XDR.TransactionV0Envelope
                XDR.TransactionV0
                    { transactionV0'memo
                    , transactionV0'operations
                    , transactionV0'sourceAccountEd25519
                    }
                signatures
            ) =
        Transaction
            { id
            , memo = memoFromXdr transactionV0'memo
            , operations = operationsFromXdr transactionV0'operations
            , signatures = signaturesFromXdr signatures
            , source =
                Address $
                StellarKey.encodePublic $
                XDR.unLengthArray transactionV0'sourceAccountEd25519
            }

    fromV1  (XDR.TransactionV1Envelope
                XDR.Transaction
                    { transaction'memo
                    , transaction'operations
                    , transaction'sourceAccount
                    }
                signatures
            ) =
        Transaction
            { id
            , memo = memoFromXdr transaction'memo
            , operations = operationsFromXdr transaction'operations
            , signatures = signaturesFromXdr signatures
            , source = addressFromXdrMuxed transaction'sourceAccount
            }

    fromFB  (XDR.FeeBumpTransactionEnvelope
                XDR.FeeBumpTransaction{feeBumpTransaction'feeSource} signatures
            ) =
        Transaction
            { id
            , memo = MemoNone
            , operations = []
            , signatures = signaturesFromXdr signatures
            , source = addressFromXdrMuxed feeBumpTransaction'feeSource
            }

    operationsFromXdr = map operationFromXdr . toList . XDR.unLengthArray

    signaturesFromXdr = toList . XDR.unLengthArray

decodeUtf8Throw :: HasCallStack => ByteString -> Text
decodeUtf8Throw = either (error . show) identity . decodeUtf8'
