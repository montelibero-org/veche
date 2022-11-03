{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
    TransactionOnChain (..),
    transactionFromDto,
    transactionFromEnvelopeXdr,
    TxId,
) where

import Prelude hiding (id)
import Prelude qualified

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.ONCRPC.XDR (xdrDeserialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Keypair qualified as StellarKey
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
    deriving (Eq, Ord, Read, Show)

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

newtype Shown a = Shown String
    deriving newtype (Eq, FromJSON, ToJSON)

instance Show (Shown a) where
    show (Shown s) = s

shown :: Show a => a -> Shown a
shown = Shown . show

data Memo = MemoNone | MemoText Text | MemoOther (Shown XDR.Memo)
    deriving (Eq, Show)

instance ToJSON Memo where
    toJSON = \case
        MemoNone    -> Aeson.Null
        MemoText  m -> Aeson.String m
        MemoOther m -> Aeson.String $ Text.pack $ show m

instance FromJSON Memo where
    parseJSON = \case
        Aeson.Null      -> pure MemoNone
        Aeson.String t  -> pure $ MemoText t
        v               -> Aeson.typeMismatch "Memo" v

memoFromXdr :: XDR.Memo -> Memo
memoFromXdr = \case
    XDR.Memo'MEMO_NONE -> MemoNone
    XDR.Memo'MEMO_TEXT text ->
        MemoText $ decodeUtf8Throw $ XDR.unLengthArray text
    memo -> MemoOther $ shown memo

data PaymentType = DirectPayment | PathPayment
    deriving (FromJSON, Generic, Read, Show, ToJSON)

data Operation
    = OperationManageData Text (Maybe Text)
    | OperationPayment
        { asset         :: Asset
        , amount        :: Scientific
        , destination   :: Address
        , source        :: Maybe Address
        , type_         :: PaymentType
        }
    | OperationChangeTrust
    | OperationCreateAccount
    | OperationCreateClaimableBalance
    | OperationSetOptions
    deriving (FromJSON, Generic, Read, Show, ToJSON)

operationFromXdr :: XDR.Operation -> Maybe Operation
operationFromXdr XDR.Operation{operation'body, operation'sourceAccount} =
    case operation'body of
        XDR.OperationBody'CHANGE_TRUST{} -> Just OperationChangeTrust
        XDR.OperationBody'CREATE_ACCOUNT{} -> Just OperationCreateAccount
        XDR.OperationBody'CREATE_CLAIMABLE_BALANCE{} ->
            Just OperationCreateClaimableBalance
        XDR.OperationBody'MANAGE_DATA (XDR.ManageDataOp name mvalue) ->
            OperationManageData
            <$> either
                    (const Nothing) Just (decodeUtf8' $ XDR.unLengthArray name)
            <*> case mvalue of
                    Nothing -> Just Nothing
                    Just array ->
                        either (const Nothing) (Just . Just) $
                        decodeUtf8' $ XDR.unLengthArray array
        XDR.OperationBody'PATH_PAYMENT_STRICT_RECEIVE
                XDR.PathPaymentStrictReceiveOp
                    { pathPaymentStrictReceiveOp'destAmount
                    , pathPaymentStrictReceiveOp'destAsset
                    , pathPaymentStrictReceiveOp'destination
                    } ->
            Just
            OperationPayment
                { amount = amountFromXdr pathPaymentStrictReceiveOp'destAmount
                , asset  = assetFromXdr  pathPaymentStrictReceiveOp'destAsset
                , destination =
                    addressFromXdrMuxed pathPaymentStrictReceiveOp'destination
                , source
                , type_ = PathPayment
                }
        XDR.OperationBody'PAYMENT
                XDR.PaymentOp
                    { paymentOp'amount
                    , paymentOp'asset
                    , paymentOp'destination
                    } ->
            Just
            OperationPayment
                { amount        = amountFromXdr         paymentOp'amount
                , asset         = assetFromXdr          paymentOp'asset
                , destination   = addressFromXdrMuxed   paymentOp'destination
                , source
                , type_ = DirectPayment
                }
        XDR.OperationBody'SET_OPTIONS{} -> Just OperationSetOptions
        _ -> Nothing
  where
    source = addressFromXdr <$> operation'sourceAccount

data Transaction = Transaction
    { memo          :: Memo
    , operations    :: [Either (Shown XDR.Operation) Operation]
    , source        :: Address
    }
    deriving (FromJSON, Generic, Show, ToJSON)

data TransactionOnChain = TransactionOnChain
    { id    :: TxId
    , time  :: UTCTime
    , tx    :: Transaction
    }
    deriving (FromJSON, Generic, ToJSON)

transactionFromDto :: HasCallStack => DTO.Transaction -> TransactionOnChain
transactionFromDto DTO.Transaction{created_at, envelope_xdr, id} =
    TransactionOnChain
        {id, time = created_at, tx = transactionFromEnvelopeXdr envelope}
  where
    envelopeXdrRaw =
        either error identity $ Base64.decode $ encodeUtf8 envelope_xdr
    envelope = either error identity $ xdrDeserialize envelopeXdrRaw

transactionFromEnvelopeXdr :: XDR.TransactionEnvelope -> Transaction
transactionFromEnvelopeXdr = \case
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
                _signatures
            ) =
        Transaction
            { memo          = memoFromXdr transactionV0'memo
            , operations    = operationsFromXdr transactionV0'operations
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
                _signatures
            ) =
        Transaction
            { memo          = memoFromXdr transaction'memo
            , operations    = operationsFromXdr transaction'operations
            , source        = addressFromXdrMuxed transaction'sourceAccount
            }

    fromFB  (XDR.FeeBumpTransactionEnvelope
                XDR.FeeBumpTransaction{feeBumpTransaction'feeSource} _signatures
            ) =
        Transaction
            { memo          = MemoNone
            , operations    = []
            , source        = addressFromXdrMuxed feeBumpTransaction'feeSource
            }

    operationsFromXdr ::
        XDR.Array n XDR.Operation -> [Either (Shown XDR.Operation) Operation]
    operationsFromXdr =
        map (\xop -> maybe (Left $ shown xop) Right $ operationFromXdr xop)
        . toList
        . XDR.unLengthArray

decodeUtf8Throw :: HasCallStack => ByteString -> Text
decodeUtf8Throw = either (error . show) identity . decodeUtf8'
