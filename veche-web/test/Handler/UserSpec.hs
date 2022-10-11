{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.UserSpec (spec) where

import TestImport

import Data.Text qualified as Text
import Stellar.Horizon.Types qualified as Stellar

import Model.User (User (User))
import Model.User qualified

spec :: Spec
spec =
    withApp $
    describe "User page" do
        it "asserts no access to my-account for anonymous users" do
            get UserR
            statusIs 403

        it "asserts access to my-account for authenticated users" do
            userEntity <- createUser "foo" Nothing
            authenticateAs userEntity

            get UserR
            statusIs 200

        it "asserts user's information is shown" do
            userEntity <- createUser "bar" Nothing
            authenticateAs userEntity

            get UserR
            let Entity _ User{stellarAddress = Stellar.Address address} =
                    userEntity
            htmlAnyContain ".user_stellar_address" $ Text.unpack address
