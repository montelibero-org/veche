{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Attachment (
    deleteTx, getTx, replaceTx, updateTx, signerKeyHint
) where

-- prelude
import Import hiding (deleteBy)

-- global
import Data.ByteString qualified as BS
import Data.Foldable (foldl)
import Data.Text qualified as Text
import Database.Persist (deleteBy, selectFirst, upsertBy, (=.), (==.))
import Network.ONCRPC.XDR (unLengthArray, xdrDeserialize, xdrSerialize)
import Network.ONCRPC.XDR qualified as XDR
import Network.Stellar.Keypair (decodePublic')
import Network.Stellar.TransactionXdr qualified as XDR
import Stellar.Simple qualified as Stellar
import Yesod.Persist (runDB)

-- component
import Genesis (mtlFund)
import Model (AttachmentTx (AttachmentTx), IssueId, StellarSigner,
              Unique (UniqueTx), UserId)
import Model qualified
import Model.StellarSigner qualified as StellarSigner

getTx :: MonadIO m => IssueId -> SqlPersistT m (Maybe TransactionBin)
getTx issue = do
    mAttachment <- selectFirst [#issue ==. issue] []
    for mAttachment \(Entity _ AttachmentTx{code}) -> pure code

replaceTx :: MonadIO m => AttachmentTx -> SqlPersistT m ()
replaceTx tx@AttachmentTx{issue, code, updated, updatedBy} =
    void $
    upsertBy
        (UniqueTx issue)
        tx
        [#code =. code, #updated =. updated, #updatedBy =. updatedBy]

deleteTx :: MonadIO m => IssueId -> SqlPersistT m ()
deleteTx issue = deleteBy $ UniqueTx issue

type Signatures = XDR.Array 20 XDR.DecoratedSignature

signerKeyHint :: StellarSigner -> ByteString
signerKeyHint =
    BS.takeEnd 4 . decodePublic' . unwrap Stellar.Address . StellarSigner.key

updateTx :: UserId -> IssueId -> TransactionB64 -> Handler Void
updateTx authenticatedUser issue newEnvelopeXdrBase64 = do
    newEnvelopeXdr <-
        case decodeTxBase64 newEnvelopeXdrBase64 of
            Right t -> pure t
            Left e  -> invalidArgs [Text.pack e]
    oldEnvelopeXdr <-
        runDB (getTx issue)
        >>= \case
            Just t  -> pure t
            Nothing -> invalidArgs ["No saved transaction"]
    when (oldEnvelopeXdr == newEnvelopeXdr) goBack
    oldEnvelope <-
        case xdrDeserialize $ unwrap TransactionBin oldEnvelopeXdr of
            Right e -> pure e
            Left e  -> invalidArgs [Text.pack e]
    newEnvelope <-
        case xdrDeserialize $ unwrap TransactionBin newEnvelopeXdr of
            Right e -> pure e
            Left e  -> invalidArgs [Text.pack e]

    (oldSignatures, newSignatures) <- checkTxsEqual (oldEnvelope, newEnvelope)
    signersHints <-
        map (signerKeyHint . entityVal) <$> StellarSigner.getAll mtlFund
    let haveExtraSignatures =
            or  [ unLengthArray hint `notElem` signersHints
                | XDR.DecoratedSignature hint _ <-
                    toList $ unLengthArray newSignatures
                ]
    when haveExtraSignatures $ invalidArgs ["Non-signer is not allowed"]

    resultSignatures <-
        case mergeSignatures oldSignatures newSignatures of
            Right r -> pure r
            Left e  -> invalidArgs [e]
    let resultEnvelope = updateSignatures resultSignatures oldEnvelope
    when (oldSignatures == resultSignatures) goBack

    let code = TransactionBin $ xdrSerialize resultEnvelope
        updatedBy = authenticatedUser
    updated <- liftIO getCurrentTime
    runDB $ replaceTx AttachmentTx{issue, code, updated, updatedBy}
    goBack

  where

    goBack = redirect $ IssueR issue

    checkTxsEqual = \case
        (       XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_V0 envelopeA
            ,   XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_V0 envelopeB
            )
            -> checkTxsEqualV0 envelopeA envelopeB
        (       XDR.TransactionEnvelope'ENVELOPE_TYPE_TX envelopeA
            ,   XDR.TransactionEnvelope'ENVELOPE_TYPE_TX envelopeB
            )
            -> checkTxsEqualV1 envelopeA envelopeB
        (       XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP envelopeA
            ,   XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP envelopeB
            )
            -> checkTxsEqualFB envelopeA envelopeB
        _ -> invalidArgs ["Transaction differs"]

    checkTxsEqualV0
            (XDR.TransactionV0Envelope txA signaturesA)
            (XDR.TransactionV0Envelope txB signaturesB)
        | txA == txB = pure (signaturesA, signaturesB)
        | otherwise  = invalidArgs ["Bad transaction"]

    checkTxsEqualV1
            (XDR.TransactionV1Envelope txA signaturesA)
            (XDR.TransactionV1Envelope txB signaturesB)
        | txA == txB = pure (signaturesA, signaturesB)
        | otherwise  = invalidArgs ["Bad transaction"]

    checkTxsEqualFB
            (XDR.FeeBumpTransactionEnvelope txA signaturesA)
            (XDR.FeeBumpTransactionEnvelope txB signaturesB)
        | txA == txB = pure (signaturesA, signaturesB)
        | otherwise  = invalidArgs ["Bad transaction"]

    mergeSignatures :: Signatures -> Signatures -> Either Text Signatures
    mergeSignatures oldSignatures newSignatures =
        maybe (Left "Too many signatures") Right . XDR.lengthArray $
        foldl
            (\acc sig -> if sig `notElem` old then acc `snoc` sig else acc)
            old
            (unLengthArray newSignatures)
      where
        old = unLengthArray oldSignatures

    updateSignatures signatures = \case
        XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_V0
                (XDR.TransactionV0Envelope tx _)
            -> XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_V0 $
                XDR.TransactionV0Envelope tx signatures
        XDR.TransactionEnvelope'ENVELOPE_TYPE_TX
                (XDR.TransactionV1Envelope tx _)
            -> XDR.TransactionEnvelope'ENVELOPE_TYPE_TX $
                XDR.TransactionV1Envelope tx signatures
        XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP
                (XDR.FeeBumpTransactionEnvelope tx _)
            -> XDR.TransactionEnvelope'ENVELOPE_TYPE_TX_FEE_BUMP $
                XDR.FeeBumpTransactionEnvelope tx signatures
