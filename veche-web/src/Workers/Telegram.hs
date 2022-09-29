{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE NumericUnderscores #-}

module Workers.Telegram (telegramBot) where

import Import

-- import Control.Concurrent (threadDelay)
import Database.Persist.Sql -- (ConnectionPool, runSqlPool)
import Network.HTTP.Client (Manager)
-- import System.Random (randomRIO)
-- import Web.Telegram.API.Bot (ChatId (ChatId), TelegramClient, Token (Token),
--                              sendMessageM, sendMessageRequest)
-- import Web.Telegram.API.Bot qualified as Telegram

-- import Model.Notification (dbDelete, dbSelectAll)

telegramBot :: ConnectionPool -> Text -> Manager -> IO ()
telegramBot _connPool _token _manager =
    pure ()
--     forever do
--         notifications <- runDB dbSelectAll
--         for_ notifications \(Entity msgId msg, mUserTelegram) -> do
--             case mUserTelegram of
--                 Just tg -> runTelegramClient' $ notify tg msg
--                 Nothing -> pure ()
--             runDB $ dbDelete msgId
--         randomDelay
--   where

--     randomDelay = do
--         delaySeconds <- randomRIO (1, 10)
--         threadDelay $ delaySeconds * 1_000_000

--     runDB = (`runSqlPool` connPool)

--     runTelegramClient' :: TelegramClient a -> IO a
--     runTelegramClient' =
--         Telegram.runTelegramClient (Token token) manager >=> either throwIO pure

-- notify :: Telegram -> Notification -> TelegramClient ()
-- notify Telegram{chatid} Notification{text} =
--     void $ sendMessageM $ sendMessageRequest (ChatId chatid) text
