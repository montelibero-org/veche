{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.UserSpec (spec) where

import TestImport

spec :: Spec
spec =
    withApp do

        describe "User page" do
            it "asserts no access to my-account for anonymous users" do
                get UserR
                statusIs 403

            it "asserts access to my-account for authenticated users" do
                userEntity <- createUser "foo"
                authenticateAs userEntity

                get UserR
                statusIs 200

            it "asserts user's information is shown" do
                userEntity <- createUser "bar"
                authenticateAs userEntity

                get UserR
                let (Entity _ User{userStellarAddress}) = userEntity
                htmlAnyContain ".user_stellar_address" $
                    unpack userStellarAddress
