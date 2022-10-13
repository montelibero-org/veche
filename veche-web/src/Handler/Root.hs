module Handler.Root (getRootR) where

import Import

getRootR :: Handler Void
getRootR = do
    muid <- maybeAuthId
    case muid of
        Just _  -> redirect DashboardR
        Nothing -> redirect AboutR
