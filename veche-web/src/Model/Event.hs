{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Model.Event (
    dbGetUndelivered,
    dbGetUsersToDeliver,
    dbSetDelivered,
    makeEventIssueClosed,
    makeEventIssueCreated,
    makeEventIssueReopened,
) where

import Import

import Database.Persist (getEntity, selectList, update, (=.), (==.))
import Yesod.Persist (get404)

makeIssueEvent :: EventType -> UTCTime -> IssueId -> Event
makeIssueEvent type_ time issue =
    Event{type_, time, delivered = False, issue = Just issue, comment = Nothing}

makeEventIssueCreated :: UTCTime -> IssueId -> Event
makeEventIssueCreated = makeIssueEvent IssueCreated

makeEventIssueClosed :: UTCTime -> IssueId -> Event
makeEventIssueClosed = makeIssueEvent IssueClosed

makeEventIssueReopened :: UTCTime -> IssueId -> Event
makeEventIssueReopened = makeIssueEvent IssueReopened

dbGetUndelivered :: MonadIO m => SqlPersistT m [Entity Event]
dbGetUndelivered = selectList [Event_delivered ==. False] []

dbSetDelivered :: MonadIO m => EventId -> SqlPersistT m ()
dbSetDelivered id = update id [Event_delivered =. True]

dbGetUsersToDeliver :: MonadIO m => Event -> SqlPersistT m [(UserId, Telegram)]
dbGetUsersToDeliver Event{type_, issue} =
    case type_ of
        IssueCreated    -> selectAll
        IssueClosed     -> getIssueAuthor
        IssueReopened   -> getIssueAuthor
  where

    selectAll = map unwrapEntity <$> selectList @Telegram [] []

    getIssueAuthor = do
        mmTelegram <-
            for issue \issueId -> do
                Issue{author} <- get404 issueId
                getEntity $ TelegramKey author
        pure $ maybeToList $ unwrapEntity <$> join mmTelegram

    unwrapEntity (Entity (TelegramKey userId) telegram) = (userId, telegram)

-- trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
-- trimap fa fb fc (a, b, c) = (fa a, fb b, fc c)
