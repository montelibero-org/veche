module Model.Telegram (
    -- * Telegram account binding
    EntityField (Telegram_chatid),
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
import Model (EntityField (..), Key (TelegramKey), Telegram (..), TelegramId,
              TelegramState (..))

dbGetState :: MonadIO m => SqlPersistT m TelegramState
dbGetState = do
    mresult <- selectFirst [] []
    case mresult of
        Nothing -> error "state must be initialized with migration"
        Just (Entity _ entityVal) -> pure entityVal

dbSetOffset :: MonadIO m => Int -> SqlPersistT m ()
dbSetOffset offset = updateWhere [] [TelegramState_offset =. Just offset]
