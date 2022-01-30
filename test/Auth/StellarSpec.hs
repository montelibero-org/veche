{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.StellarSpec (spec) where

import TestImport

spec :: Spec
spec = withApp do
    describe "Auth.Stellar" do
        it "shows pubic key form" do
            get $ AuthR LoginR
            statusIs 200
            htmlCount "input[name=stellar_address]" 1
