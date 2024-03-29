{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pretty (prettyEnvelope) where

-- prelude
import Import hiding (Key)
import Network.Stellar.TransactionXdr hiding (Int64)

-- global
import Data.Aeson (Key, Object, Value (Null, Object, String), toJSON)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (insert)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base32 (encodeBase32)
import Data.Scientific (FPFormat (Fixed), formatScientific, scientific)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (replace)
import Data.Yaml.Pretty (defConfig, encodePretty)
import Network.ONCRPC.XDR (Array, LengthArray (unLengthArray), XDRDiscriminant,
                           XDRUnion, unLengthArray, xdrSplitUnion)
import Network.Stellar.Keypair (encodePublic)

prettyEnvelope :: TransactionEnvelope -> Text
prettyEnvelope =
    decodeUtf8Throw
    . encodePretty defConfig
    . \case
        TransactionEnvelope'ENVELOPE_TYPE_TX_V0 (TransactionV0Envelope tx sigs)
            ->  Object $
                insert "type" "v0" $
                insert "signatures" (prettySignatures sigs) $
                prettyTransactionV0 tx
        TransactionEnvelope'ENVELOPE_TYPE_TX (TransactionV1Envelope tx sigs) ->
            Object $
            insert "type" "v1" $
            insert "signatures" (prettySignatures sigs) $
            prettyTransactionV1 tx
        TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP
                (FeeBumpTransactionEnvelope tx sigs)
            ->  object
                    [ "type"        .= String "fee bump"
                    , "transaction" .= tshow tx
                    , "signatures"  .= prettySignatures sigs
                    ]

prettySignatures :: Array 20 DecoratedSignature -> Value
prettySignatures = array . map prettySignature . toList . unLengthArray

prettySignature :: DecoratedSignature -> Value
prettySignature (DecoratedSignature hint _) =
    String $
        "G..."
        <> take 5 (drop 7 $ encodeBase32 $ "\0\0\0\0" <> unLengthArray hint)
        <> "..."

prettyTransactionV0 :: TransactionV0 -> Object
prettyTransactionV0 TransactionV0{..} = KeyMap.filter (/= Null) $
    KeyMap.fromList
        [ "main account" .= prettyPublicKey' transactionV0'sourceAccountEd25519
        , "fee"          .= prettyStroops transactionV0'fee
        , "memo"         .= prettyMemo transactionV0'memo
        , "operations"   .= prettyOperations transactionV0'operations
        , "time bounds"  .= show transactionV0'timeBounds
        ]

prettyTransactionV1 :: Transaction -> Object
prettyTransactionV1 Transaction{..} =
    KeyMap.filter (/= Null) $
    KeyMap.fromList
        [ "main account"  .= prettyMuxedAddress transaction'sourceAccount
        , "preconditions" .= prettyCond transaction'cond
        , "fee"           .= prettyStroops transaction'fee
        , "memo"          .= prettyMemo transaction'memo
        , "operations"    .= prettyOperations transaction'operations
        ]

prettyOperations :: Array 100 Operation -> [Object]
prettyOperations = map prettyOperation . toList . unLengthArray

prettyCond :: Preconditions -> Value
prettyCond = \case
    Preconditions'PRECOND_NONE -> Null
    c -> String $ tshow c

prettyMemo :: Memo -> Value
prettyMemo = \case
    Memo'MEMO_NONE -> Null
    m -> String $ tshow m

prettyOperation :: Operation -> Object
prettyOperation (Operation sourceAccount body) =
    fromText (tshow $ xdrUnionType body) .= prettyOperationBody body
    & maybe
        identity
        (insert "source account" . String . prettyPublicKey)
        sourceAccount

prettyOperationBody :: OperationBody -> Object
prettyOperationBody = \case
    OperationBody'CHANGE_TRUST   op -> prettyChangeTrust   op
    OperationBody'CREATE_ACCOUNT op -> prettyCreateAccount op
    OperationBody'PAYMENT        op -> prettyPayment       op
    OperationBody'SET_OPTIONS    op -> prettySetOptions    op
    unknown -> KeyMap.fromList ["unknown_op" .= show unknown]

xdrUnionType :: XDRUnion a => a -> XDRDiscriminant a
xdrUnionType = toEnum . fromIntegral . fst . xdrSplitUnion

prettyChangeTrust :: ChangeTrustOp -> Object
prettyChangeTrust ChangeTrustOp{..} =
    KeyMap.fromList
        [ "line"  .= prettyAsset changeTrustOp'line
        , "limit" .=
            if changeTrustOp'limit == maxBound then
                "UNLIMITED"
            else
                toJSON changeTrustOp'limit
        ]

prettyAsset :: Asset -> Text
prettyAsset = \case
    Asset'ASSET_TYPE_NATIVE                                   -> "XLM"
    Asset'ASSET_TYPE_CREDIT_ALPHANUM4  (AlphaNum4  code issr) -> go code issr
    Asset'ASSET_TYPE_CREDIT_ALPHANUM12 (AlphaNum12 code issr) -> go code issr
  where
    go code issuer =
        decodeAsciizOptimistically (unLengthArray code)
        <> "-"
        <> prettyPublicKey issuer

decodeAsciizOptimistically :: ByteString -> Text
decodeAsciizOptimistically = decodeUtf8With (replace '�') . takeWhile (/= 0)

prettyCreateAccount :: CreateAccountOp -> Object
prettyCreateAccount CreateAccountOp{..} =
    KeyMap.fromList
        [ "destination"      .= prettyPublicKey createAccountOp'destination
        , "starting balance" .= prettyStroops createAccountOp'startingBalance
        ]

prettyPayment :: PaymentOp -> Object
prettyPayment PaymentOp{..} =
    KeyMap.fromList
        [ "destination" .= prettyMuxedAddress paymentOp'destination
        , "asset"       .= prettyAsset paymentOp'asset
        , "amount"      .= prettyAssetAmount paymentOp'amount
        ]

prettySetOptions :: SetOptionsOp -> Object
prettySetOptions SetOptionsOp{..} =
    KeyMap.filter (/= Null) $
    KeyMap.fromList
        [ "clear flags"        .= setOptionsOp'clearFlags
        , "set high threshold" .= setOptionsOp'highThreshold
        , "set home domain"    .=
            (   decodeAsciizOptimistically . unLengthArray
            <$> setOptionsOp'homeDomain
            )
        , "set inflation dest" .= (show <$> setOptionsOp'inflationDest)
        , "set low threshold"  .= setOptionsOp'lowThreshold
        , "set master weight"  .= setOptionsOp'masterWeight
        , "set med threshold"  .= setOptionsOp'medThreshold
        , "set flags"          .= setOptionsOp'setFlags
        , maybe ("NO SET SIGNER" .= Null) prettySigner setOptionsOp'signer
        ]

prettySigner :: Signer -> (Key, Value)
prettySigner = \case
    Signer (SignerKey'SIGNER_KEY_TYPE_ED25519 account) 0 ->
        "remove signer" .= prettyPublicKey' account
    Signer (SignerKey'SIGNER_KEY_TYPE_ED25519 account) weight ->
        "add/change signer"
        .= object [fromText (prettyPublicKey' account) .= weight]
    Signer key 0 -> "remove signer" .= show key
    Signer key weight ->
        "add/change signer" .= object ["key" .= show key, "weight" .= weight]

prettyPublicKey :: PublicKey -> Text
prettyPublicKey (PublicKey'PUBLIC_KEY_TYPE_ED25519 account) =
    prettyPublicKey' account

prettyPublicKey' :: Uint256 -> Text
prettyPublicKey' = encodePublic . unLengthArray

prettyMuxedAddress :: MuxedAccount -> Value
prettyMuxedAddress = \case
    MuxedAccount'KEY_TYPE_ED25519 account -> String $ prettyPublicKey' account
    m -> String $ tshow m

prettyStroops :: (Integral i, Show i) => i -> Text
prettyStroops stroops =
    mconcat [formatTenmillionths stroops, " XLM (", tshow stroops, " stroops)"]

prettyAssetAmount :: Int64 -> Text
prettyAssetAmount amount =
    mconcat [formatTenmillionths amount, " (raw: ", tshow amount, ")"]

formatTenmillionths :: Integral i => i -> Text
formatTenmillionths x =
    Text.pack $ formatScientific Fixed Nothing $ scientific (toInteger x) (-7)
