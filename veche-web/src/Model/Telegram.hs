module Model.Telegram (
    -- * Telegram account binding
    Key (TelegramKey),
    Telegram (..),
    TelegramId,
    -- * Telegram bot state
    TelegramState (..),
    dbGetState,
    dbSetOffset,
) where

-- prelude
import Import.NoFoundation

-- global
import Database.Persist (selectFirst, updateWhere, (=.))

-- component
import Model (EntityField (TelegramState_offset), Key (TelegramKey),
              Telegram (..), TelegramId, TelegramState (..))

emptyState :: TelegramState
emptyState = TelegramState{offset = Nothing}

dbGetState :: MonadIO m => SqlPersistT m TelegramState
dbGetState = selectFirst [] [] <&> maybe emptyState entityVal

dbSetOffset :: MonadIO m => Int -> SqlPersistT m ()
dbSetOffset offset = updateWhere [] [TelegramState_offset =. Just offset]
