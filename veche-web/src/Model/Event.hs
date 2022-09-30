-- {-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}

module Model.Event (
--     dbDelete,
    dbSelectAll,
) where

import Import

import Database.Persist (selectList)
-- import Database.Persist.Sql (Single, rawSql, unSingle)
-- import Prelude (id)

-- dbDelete :: MonadIO m => NotificationId -> SqlPersistT m ()
-- dbDelete = delete

dbSelectAll :: MonadIO m => SqlPersistT m [Event]
dbSelectAll = map entityVal <$> selectList [] []

-- trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
-- trimap fa fb fc (a, b, c) = (fa a, fb b, fc c)
