{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Model.Attachment (getTx) where

-- prelude
import Import

-- global
import Database.Persist (selectFirst, (==.))

-- component
import Model (AttachmentTx (AttachmentTx), IssueId)
import Model qualified

getTx :: MonadIO m => IssueId -> SqlPersistT m (Maybe TransactionEncoded)
getTx issue = do
    mAttachment <- selectFirst [#issue ==. issue] []
    for mAttachment \(Entity _ AttachmentTx{code}) -> pure code
