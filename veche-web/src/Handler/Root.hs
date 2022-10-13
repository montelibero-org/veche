module Handler.Root (getRootR) where

import Import

getRootR :: Handler Void
getRootR = redirect DashboardR
