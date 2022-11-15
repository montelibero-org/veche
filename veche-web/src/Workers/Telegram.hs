{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Workers.Telegram (telegramBot) where

-- prelude
import Import hiding (filter, for_)
import Prelude (filter)

-- global
import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Database.Persist.Sql (runSqlPool)
import Network.HTTP.Types (forbidden403)
import Servant.Client (ClientError (FailureResponse), ResponseF (Response))
import Servant.Client qualified
import System.Random (randomRIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Telegram.API.Bot (ChatId (ChatId), ParseMode (HTML), TelegramClient,
                             Token (Token), getUpdatesM, getUpdatesRequest,
                             message_parse_mode, sendMessageM,
                             sendMessageRequest)
import Web.Telegram.API.Bot qualified as Telegram
import Yesod.Core (messageLoggerSource, renderMessage)

-- component
import Model.Event (SomeEvent (SomeEvent))
import Model.Event qualified as Event
import Model.Telegram (Telegram (Telegram), TelegramState (TelegramState),
                       dbGetState, dbSetOffset)
import Model.Telegram qualified
import Model.User qualified as User

telegramBot :: App -> IO ()
telegramBot app =
    runLogger do
        $logInfo "Started Telegram bot"
        forever do
            getAndHandleTelegramUpdates
            deliverEvents
            randomDelay

  where

    runLogger = (`runLoggingT` messageLoggerSource app appLogger)

    getAndHandleTelegramUpdates = do
        offset <- runDB dbGetState <&> \TelegramState{..} -> offset
        Telegram.Response{result = updates} <-
            runTelegramClientThrow $
            getUpdatesM
                getUpdatesRequest{Telegram.updates_offset = succ <$> offset}
        for_ updates \update@Telegram.Update{update_id} -> do
            handleUpdate update
            runDB $ dbSetOffset update_id

    handleUpdate update@Telegram.Update{message}
        | Just m <- message = handleMessage m
        | otherwise = $logError $ "unhandled " <> tshow update

    handleMessage msg@Telegram.Message{chat, from, text}
        | Just t <- text = handleMessageText lang (ChatId chat_id) t
        | otherwise = $logError $ "unhandled " <> tshow msg
      where
        Telegram.Chat{chat_id} = chat
        lang = do
            Telegram.User{user_language_code} <- from
            Telegram.LanguageCode code <- user_language_code
            pure code

    handleMessageText lang chatId = \case
        "/help"     -> sendHelp
        "/start"    -> sendHelp
        _           -> sendMsg MsgTelegramBotNotUnderstood
      where
        sendHelp = sendMsg $ MsgTelegramBotHelp appRoot
        sendMsg msg =
            void $
            runTelegramClientThrow $
            sendMessageM $ sendMessageRequest chatId $ tr lang msg

    tr = renderMessage app . maybeToList

    deliverEvents = do
        events <- runDB Event.dbGetUndelivered
        for_ events deliver

    deliver :: SomeEvent -> LoggingT IO ()
    deliver (SomeEvent eventE@(Entity id event)) = do
        users <- runDB $ Event.dbGetUsersToDeliver event
        notifyAll eventE $ filterWhitelist users
        runDB $ Event.dbSetDelivered id

    filterWhitelist
        | [] <- whitelist   = identity
        | otherwise         =
            filter \(_, Telegram{username}) -> username `elem` whitelist

    App{appConnPool, appHttpManager, appLogger, appSettings} = app

    AppSettings
            { appRoot
            , appTelegramBotToken
            , appTelegramBotNotifyWhitelist = whitelist
            } =
        appSettings

    randomDelay =
        liftIO do
            delaySeconds <- randomRIO (1, 10)
            threadDelay $ delaySeconds * 1_000_000

    runDB = (`runSqlPool` appConnPool)

    notifyAll event users =
        for_ users \(userId, telegram) -> do
            msg <- runDB $ Event.makeMessage app event
            result <-
                runTelegramClient $ notify telegram $ toStrict $ renderHtml msg
            case result of
                Left (FailureResponse _ Response{responseStatusCode})
                    | responseStatusCode == forbidden403 ->
                        runDB $ User.dbDeleteTelegram userId
                Left e -> throwIO $ userError $ show (e, event, renderHtml msg)
                Right () -> pure ()

    runTelegramClient =
        liftIO
        . Telegram.runTelegramClient
            (Token $ "bot" <> appTelegramBotToken)
            appHttpManager

    runTelegramClientThrow =
        runTelegramClient >=> either throwWithCallStackIO pure

notify :: Telegram -> Text -> TelegramClient ()
notify Telegram{chatid} text =
    void $
    sendMessageM
        (sendMessageRequest (ChatId chatid) text)
            {message_parse_mode = Just HTML}
