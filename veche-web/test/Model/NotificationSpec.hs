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
            Entity alfa _  <- createUser "alfa_has"   Nothing
            _              <- createUser "bravo_no"   Nothing
            Entity delta _ <- createUser "delta_has"  (Just (25, "delta_tg"))
            _              <- createUser "charlie_no" (Just (27, "charlie_tg"))
            Entity echo _  <- createUser "echo_has"   (Just (26, "echo_tg"))
            notifications <-
                runDB do
                    Notification.dbInsert alfa  "twenty four"
                    Notification.dbInsert delta "twenty five"
                    Notification.dbInsert echo  "twenty six"
                    Notification.dbSelectAll <&> map (first entityVal)
            notifications === expectNotifications alfa delta echo
  where
    expectNotifications alfa delta echo =
        [   (Notification{recipient = alfa, text = "twenty four"}, Nothing)
        ,   ( Notification{recipient = delta, text = "twenty five"}
            , Just Telegram{chatid = 25, username = "delta_tg"}
            )
        ,   ( Notification{recipient = echo, text = "twenty six"}
            , Just Telegram{chatid = 26, username = "echo_tg"}
            )
        ]

(===) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
a === b =
    void $
    Hedgehog.check $ Hedgehog.withTests 1 $ Hedgehog.property $ a Hedgehog.=== b
