{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.NotificationSpec (spec) where

import TestImport

import Hedgehog qualified

import Model.Notification qualified as Notification

spec :: Spec
spec =
    withApp $
    describe "Model.Notification" do
        it "selectAll" do
            Entity alfa _  <- createUser "alfa_has"
            _              <- createUser "bravo_no"
            Entity delta _ <- createUser "delta_has"
            _              <- createUser "charlie_no"
            Entity echo _  <- createUser "echo_has"
            notifications <-
                runDB do
                    Notification.dbInsert alfa  "twenty four"
                    Notification.dbInsert delta "twenty five"
                    Notification.dbInsert echo  "twenty six"
                    Notification.dbSelectAll <&> map entityVal
            notifications === expectNotifications alfa delta echo
  where
    expectNotifications alfa delta echo =
        [   Notification
                {notificationRecipient = alfa, notificationText = "twenty four"}
        ,   Notification
                { notificationRecipient = delta
                , notificationText = "twenty five"
                }
        ,   Notification
                {notificationRecipient = echo, notificationText = "twenty six"}
        ]

(===) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
a === b =
    void $
    Hedgehog.check $ Hedgehog.withTests 1 $ Hedgehog.property $ a Hedgehog.=== b
