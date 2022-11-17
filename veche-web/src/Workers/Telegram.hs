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
                             inlineKeyboardButton, loginUrl, message_parse_mode,
                             sendMessageM, sendMessageRequest)
import Web.Telegram.API.Bot qualified as Telegram
import Yesod.Core (messageLoggerSource, renderMessage, yesodRender)

-- component
import Authentication.Telegram qualified as Authn
import Model.Event (Event, SomeEvent (SomeEvent))
import Model.Event qualified as Event
import Model.Telegram (Telegram (Telegram), TelegramState (TelegramState),
                       dbGetState, dbSetOffset)
import Model.Telegram qualified
import Model.User (UserId)
import Model.User qualified as User

telegramBot :: App -> IO ()
telegramBot app =
    (`runReaderT` app) $
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

    handleUpdate ::
        (MonadIO m, MonadLogger m, MonadReader App m) => Telegram.Update -> m ()
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
        sendHelp = do
            authUrl <- renderUrl $ AuthR Authn.pluginRoute
            void $
                runTelegramClientThrow $
                sendMessageM
                    ( sendMessageRequest chatId $
                        tr lang $ MsgTelegramBotHelp appRoot
                    )
                    { Telegram.message_reply_markup =
                        Just $
                        Telegram.ReplyInlineKeyboardMarkup
                            [   [   ( inlineKeyboardButton $
                                        tr lang MsgTelegramBotLoginButton
                                    )
                                    { Telegram.ikb_login_url =
                                        Just $ loginUrl authUrl
                                    }
                                ]
                            ]
                    }
        sendMsg msg =
            void $
            runTelegramClientThrow $
            sendMessageM $ sendMessageRequest chatId $ tr lang msg

    tr = renderMessage app . maybeToList

    App{appLogger, appSettings = AppSettings{appRoot}} = app

deliverEvents :: (MonadReader App m, MonadUnliftIO m) => m ()
deliverEvents = do
    events <- runDB Event.dbGetUndelivered
    for_ events deliver

deliver :: (MonadReader App m, MonadUnliftIO m) => SomeEvent -> m ()
deliver (SomeEvent eventE@(Entity id event)) = do
    App{appSettings} <- ask
    let AppSettings{appTelegramBotNotifyWhitelist = whitelist} = appSettings
    let filterWhitelist
            | [] <- whitelist = identity
            | otherwise =
                filter \(_, Telegram{username}) -> username `elem` whitelist
    users <- runDB $ Event.dbGetUsersToDeliver event
    notifyAll eventE $ filterWhitelist users
    runDB $ Event.dbSetDelivered id

randomDelay :: MonadIO m => m ()
randomDelay =
    liftIO do
        delaySeconds <- randomRIO (1, 10)
        threadDelay $ delaySeconds * 1_000_000

runDB :: (MonadReader App m, MonadUnliftIO m) => SqlPersistT m a -> m a
runDB action = do
    App{appConnPool} <- ask
    (`runSqlPool` appConnPool) action

notifyAll ::
    (MonadReader App m, MonadUnliftIO m, Event e) =>
    Entity e -> [(UserId, Telegram)] -> m ()
notifyAll event users = do
    app <- ask
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

runTelegramClient ::
    (MonadReader App m, MonadIO m) =>
    TelegramClient a -> m (Either ClientError a)
runTelegramClient action = do
    App{appHttpManager, appSettings = AppSettings{appTelegramBotToken}} <- ask
    liftIO $
        Telegram.runTelegramClient
            (Token $ "bot" <> appTelegramBotToken)
            appHttpManager
            action

runTelegramClientThrow ::
    (MonadIO m, MonadReader App m) => TelegramClient a -> m a
runTelegramClientThrow =
    runTelegramClient >=> either throwWithCallStackIO pure

renderUrl :: MonadReader App m => Route App -> m Text
renderUrl route = do
    app@App{appSettings = AppSettings{appRoot}} <- ask
    pure $ yesodRender app appRoot route []

notify :: Telegram -> Text -> TelegramClient ()
notify Telegram{chatid} text =
    void $
    sendMessageM
        (sendMessageRequest (ChatId chatid) text)
            {message_parse_mode = Just HTML}
