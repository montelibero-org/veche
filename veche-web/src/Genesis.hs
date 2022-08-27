{-# LANGUAGE OverloadedStrings #-}

module Genesis where

import Import

import Stellar.Horizon.Types (Asset (Asset))

mtlAsset :: Asset
mtlAsset =
    Asset "MTL" "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"

mtlFund :: Text
mtlFund = "GDX23CPGMQ4LN55VGEDVFZPAJMAUEHSHAMJ2GMCU2ZSHN5QF4TMZYPIS"
