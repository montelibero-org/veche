{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Workers.Telegram (telegramBot) where

import Import hiding (for_, link)

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Database.Persist.Sql (runSqlPool)
import Network.HTTP.Types (forbidden403)
import Servant.Client (ClientError (FailureResponse), ResponseF (Response))
import Servant.Client qualified
import System.Random (randomRIO)
import Web.Telegram.API.Bot (ChatId (ChatId), TelegramClient, Token (Token),
                             sendMessageM, sendMessageRequest)
import Web.Telegram.API.Bot qualified as Telegram

import Model.Event (SomeEvent (SomeEvent))
import Model.Event qualified as Event
import Model.User qualified as User

telegramBot :: App -> IO ()
telegramBot app = do
    putStrLn "Started Telegram bot"
    forever do
        events <- runDB Event.dbGetUndelivered
        for_ events deliver
        randomDelay

  where

    deliver :: SomeEvent -> IO ()
    deliver (SomeEvent eventE@(Entity id event)) = do
        users <- runDB $ Event.dbGetUsersToDeliver event
        notifyAll eventE users
        runDB $ Event.dbSetDelivered id

    App{appConnPool, appSettings, appHttpManager} = app
    AppSettings{appTelegramBotToken} = appSettings

    randomDelay = do
        delaySeconds <- randomRIO (1, 10)
        threadDelay $ delaySeconds * 1_000_000

    runDB = (`runSqlPool` appConnPool)

    notifyAll event users =
        for_ users \(userId, telegram) -> do
            result <-
                runTelegramClient' $
                notify (Event.makeMessage app event) telegram
            case result of
                Left (FailureResponse _ Response{responseStatusCode})
                    | responseStatusCode == forbidden403 ->
                        runDB $ User.dbDeleteTelegram userId
                Left e ->
                    throwIO $
                    userError $ show (e, event, Event.makeMessage app event)
                Right () -> pure ()

    runTelegramClient' =
        Telegram.runTelegramClient
            (Token $ "bot" <> appTelegramBotToken)
            appHttpManager

notify :: Text -> Telegram -> TelegramClient ()
notify text Telegram{chatid} =
    void $ sendMessageM $ sendMessageRequest (ChatId chatid) text
