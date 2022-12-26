{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stellar.Simple.Types where

-- global
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.Stellar.TransactionXdr qualified as XDR
import Servant.API (FromHttpApiData, ToHttpApiData)
import Servant.API qualified

-- component
import Stellar.Horizon.DTO (Address, TxId)

newtype Shown a = Shown String
    deriving newtype (Eq, FromJSON, ToJSON)

instance Show (Shown a) where
    show (Shown s) = s

shown :: Show a => a -> Shown a
shown = Shown . show

data Asset = Asset{issuer :: Maybe Text, code :: Text}
    deriving (Eq, Generic, Ord, Read, Show)

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

data DecoratedSignature = DecoratedSignature{hint, signature :: ByteString}
    deriving Show

instance FromJSON DecoratedSignature where
    parseJSON =
        Aeson.withObject "DecoratedSignature" \obj -> do
            hint      <- obj Aeson..: "hint"      >>= decodeBase64
            signature <- obj Aeson..: "signature" >>= decodeBase64
            pure DecoratedSignature{..}
      where
        decodeBase64 = either fail pure . Base64.decode . encodeUtf8

instance ToJSON DecoratedSignature where
    toJSON (DecoratedSignature{..}) =
        Aeson.object
            [ "hint"      Aeson..= encodeBase64 hint
            , "signature" Aeson..= encodeBase64 signature
            ]
      where
        encodeBase64 = decodeUtf8 . Base64.encode

data Transaction = Transaction
    { memo          :: Memo
    , operations    :: [Either (Shown XDR.Operation) Operation]
    , source        :: Address
    , signatures    :: [DecoratedSignature]
    }
    deriving (FromJSON, Generic, Show, ToJSON)

data TransactionOnChain = TransactionOnChain
    { id    :: TxId
    , time  :: UTCTime
    , tx    :: Transaction
    }
    deriving (FromJSON, Generic, Show, ToJSON)
