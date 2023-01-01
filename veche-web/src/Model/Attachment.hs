{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Model.Attachment (deleteTx, getTx, replaceTx) where

-- prelude
import Import hiding (deleteBy)

-- global
import Database.Persist (deleteBy, selectFirst, upsertBy, (=.), (==.))

-- component
import Model (AttachmentTx (AttachmentTx), IssueId, Unique (UniqueTx))
import Model qualified

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
