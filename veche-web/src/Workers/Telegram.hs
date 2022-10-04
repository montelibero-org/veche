{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Workers.Telegram (telegramBot) where

import Import hiding (for_, link)

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Data.Text qualified as Text
import Database.Persist.Sql (runSqlPool)
import Network.HTTP.Types (forbidden403)
import Servant.Client (ClientError (FailureResponse), ResponseF (Response))
import Servant.Client qualified
import System.Random (randomRIO)
import Text.Shakespeare.Text (st)
import Web.Telegram.API.Bot (ChatId (ChatId), TelegramClient, Token (Token),
                             sendMessageM, sendMessageRequest)
import Web.Telegram.API.Bot qualified as Telegram
import Yesod.Core (yesodRender)

import Model.Event qualified as Event
import Model.Request qualified as Request
import Model.User qualified as User
import Templates.Comment (commentAnchor)

renderUrl :: App -> Route App -> Text
renderUrl app route = yesodRender app appRoot route [] where
    App{appSettings = AppSettings{appRoot}} = app

telegramBot :: App -> IO ()
telegramBot app = do
    putStrLn "Started Telegram bot"
    forever do
        events <- runDB Event.dbGetUndelivered
        for_ events \(Entity id event) -> do
            users <- runDB $ Event.dbGetUsersToDeliver event
            notifyAll event users makeEventMessage
            runDB $ Event.dbSetDelivered id

        requests <- runDB Request.dbGetUndelivered
        for_ requests \(Entity id request) -> do
            mUser <- runDB $ Request.dbGetUserToDeliver request
            notifyAll request mUser makeRequestMessage
            runDB $ Request.dbSetDelivered id

        randomDelay

  where

    App{appConnPool, appSettings, appHttpManager} = app
    AppSettings{appTelegramBotToken} = appSettings

    randomDelay = do
        delaySeconds <- randomRIO (1, 10)
        threadDelay $ delaySeconds * 1_000_000

    runDB = (`runSqlPool` appConnPool)

    notifyAll event users makeMessage =
        for_ users \(userId, telegram) -> do
            result <-
                runTelegramClient' $ notify (makeMessage app event) telegram
            case result of
                Left (FailureResponse _ Response{responseStatusCode})
                    | responseStatusCode == forbidden403 ->
                        runDB $ User.dbDeleteTelegram userId
                Left e -> throwIO e
                Right () -> pure ()

    runTelegramClient' =
        Telegram.runTelegramClient
            (Token $ "bot" <> appTelegramBotToken)
            appHttpManager

makeEventMessage :: App -> Event -> Text
makeEventMessage app event@Event{type_, issue} =
    case type_ of
        IssueCreated
            | Just issueId <- issue ->
                [st|A new discussion is started #{link $ IssueR issueId}|]
            | otherwise -> defMsg
        IssueClosed
            | Just issueId <- issue ->
                [st|Discussion is closed #{link $ IssueR issueId}|]
            | otherwise -> defMsg
        IssueReopened
            | Just issueId <- issue ->
                [st|Discussion is reopened #{link $ IssueR issueId}|]
            | otherwise -> defMsg
  where
    defMsg = Text.pack $ show event
    link = renderUrl app

makeRequestMessage :: App -> Request -> Text
makeRequestMessage app Request{issue, comment} =
    [st|Somebody requested you to comment in #{link}|]
  where
    link = renderUrl app (IssueR issue) <> "#" <> commentAnchor comment

notify :: Text -> Telegram -> TelegramClient ()
notify text Telegram{chatid} =
    void $ sendMessageM $ sendMessageRequest (ChatId chatid) text
