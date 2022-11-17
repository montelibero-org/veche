{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Database.Persist.Sql (ConnectionPool, runSqlPool)
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
import Yesod.Core (Lang, messageLoggerSource, renderMessage, yesodRender)

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
telegramBot app@App{appLogger, appSettings} =
    runLogger do
        $logInfo "Started Telegram bot"
        forever do
            let ?app = app
                ?appSettings = appSettings
            getAndHandleTelegramUpdates
            deliverEvents
            randomDelay
  where
    runLogger = (`runLoggingT` messageLoggerSource app appLogger)

getAndHandleTelegramUpdates ::
    (MonadLogger m, MonadUnliftIO m, ?app :: App) => m ()
getAndHandleTelegramUpdates = do
    let ?connectionPool = appConnPool
    offset <- runDB dbGetState <&> \TelegramState{..} -> offset
    Telegram.Response{result = updates} <-
        runTelegramClientThrow $
        getUpdatesM
            getUpdatesRequest{Telegram.updates_offset = succ <$> offset}
    for_ updates \update@Telegram.Update{update_id} -> do
        handleUpdate update
        runDB $ dbSetOffset update_id
  where
    App{appConnPool} = ?app

handleUpdate ::
    (MonadIO m, MonadLogger m, ?app :: App) => Telegram.Update -> m ()
handleUpdate update@Telegram.Update{message}
    | Just m <- message = handleMessage m
    | otherwise         = $logError $ "unhandled " <> tshow update

handleMessage ::
    (MonadIO m, MonadLogger m, ?app :: App) => Telegram.Message -> m ()
handleMessage msg@Telegram.Message{chat, from, text} = do
    let ?chatId = ChatId chat_id
        ?lang = do
            Telegram.User{user_language_code} <- from
            Telegram.LanguageCode code <- user_language_code
            pure code

    if  | Just t <- text -> handleMessageText t
        | otherwise      -> $logError $ "unhandled " <> tshow msg

  where
    Telegram.Chat{chat_id} = chat

handleMessageText ::
    (MonadIO m, ?app :: App, ?lang :: Maybe Lang, ?chatId :: ChatId) =>
    Text -> m ()
handleMessageText = \case
    "/help"     -> replyHelp
    "/start"    -> replyHelp
    _           -> replyNotUnderstood
  where

    App{appSettings = AppSettings{appRoot}} = ?app

    replyHelp =
        void $
        runTelegramClientThrow $
        sendMessageM
            (sendMessageRequest ?chatId msgHelp)
            { Telegram.message_reply_markup =
                Just $ Telegram.ReplyInlineKeyboardMarkup [[loginButton]]
            }
      where
        authUrl = renderUrl $ AuthR Authn.pluginRoute

        msgHelp        = tr $ MsgTelegramBotHelp appRoot
        msgLoginButton = tr   MsgTelegramBotLoginButton

        loginButton =
            (inlineKeyboardButton msgLoginButton)
            {Telegram.ikb_login_url = Just $ loginUrl authUrl}

    replyNotUnderstood =
        sendMsg $ sendMessageRequest ?chatId $ tr MsgTelegramBotNotUnderstood

    sendMsg = void . runTelegramClientThrow . sendMessageM

tr :: (?app :: App, ?lang :: Maybe Lang) => AppMessage -> Text
tr = renderMessage ?app $ maybeToList ?lang

deliverEvents :: (MonadUnliftIO m, ?app :: App) => m ()
deliverEvents = do
    let ?connectionPool    = appConnPool
        ?usernameWhitelist = appTelegramBotNotifyWhitelist
    events <- runDB Event.dbGetUndelivered
    for_ events deliver
  where
    App{appConnPool, appSettings} = ?app
    AppSettings{appTelegramBotNotifyWhitelist} = appSettings

deliver :: (?app :: App, MonadUnliftIO m) => SomeEvent -> m ()
deliver (SomeEvent eventE@(Entity id event)) = do
    let ?connectionPool = appConnPool
    users <- runDB $ Event.dbGetUsersToDeliver event
    notifyAll eventE $ filterWhitelist users
    runDB $ Event.dbSetDelivered id
  where

    App{appConnPool, appSettings} = ?app
    AppSettings{appTelegramBotNotifyWhitelist = whitelist} = appSettings

    filterWhitelist
        | [] <- whitelist = identity
        | otherwise =
            filter \(_, Telegram{username}) -> username `elem` whitelist

randomDelay :: MonadIO m => m ()
randomDelay =
    liftIO do
        delaySeconds <- randomRIO (1, 10)
        threadDelay $ delaySeconds * 1_000_000

runDB ::
    (?connectionPool :: ConnectionPool, MonadUnliftIO m) =>
    SqlPersistT m a -> m a
runDB = (`runSqlPool` ?connectionPool)

notifyAll ::
    (?app :: App, MonadUnliftIO m, Event e) =>
    Entity e -> [(UserId, Telegram)] -> m ()
notifyAll event users =
    for_ users \(userId, telegram) -> do
        let ?connectionPool = appConnPool
        msg <- runDB $ Event.makeMessage ?app event
        result <-
            runTelegramClient $ notify telegram $ toStrict $ renderHtml msg
        case result of
            Left (FailureResponse _ Response{responseStatusCode})
                | responseStatusCode == forbidden403 ->
                    runDB $ User.dbDeleteTelegram userId
            Left e -> throwIO $ userError $ show (e, event, renderHtml msg)
            Right () -> pure ()
  where
    App{appConnPool} = ?app

runTelegramClient ::
    (?app :: App, MonadIO m) => TelegramClient a -> m (Either ClientError a)
runTelegramClient action =
    liftIO $ Telegram.runTelegramClient token appHttpManager action
  where
    App{appHttpManager, appSettings = AppSettings{appTelegramBotToken}} = ?app
    token = Token $ "bot" <> appTelegramBotToken

runTelegramClientThrow :: (MonadIO m, ?app :: App) => TelegramClient a -> m a
runTelegramClientThrow =
    runTelegramClient >=> either throwWithCallStackIO pure

renderUrl :: (?app :: App) => Route App -> Text
renderUrl route =
    let App{appSettings = AppSettings{appRoot}} = ?app
    in yesodRender ?app appRoot route []

notify :: Telegram -> Text -> TelegramClient ()
notify Telegram{chatid} text =
    void $
    sendMessageM
        (sendMessageRequest (ChatId chatid) text)
            {message_parse_mode = Just HTML}
