{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.StellarSpec (spec) where

import TestImport

import Data.Aeson.QQ.Simple (aesonQQ)
import Yesod.Static (Route (StaticRoute))

spec :: Spec
spec =
    withApp do
        describe "toml" $
            it "is accessible without authentication" do
                get $ WellKnownR $ WKStaticR $ StaticRoute ["stellar.toml"] []
                statusIs 200
                assertHeader "Access-Control-Allow-Origin" "*"

        describe "Federation" $
            it "works" do
                get ( StellarFederationR
                    , [("type", "name"), ("q", "E12*veche.montelibero.org")]
                    )
                statusIs 200
                v <- requireJSONResponse
                v ===
                    [aesonQQ|
                        { "stellar_address": "E12*veche.montelibero.org"
                        , "account_id":
                            "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"
                        , "memo_type": "text"
                        , "memo": "E12"
                        }
                    |]
