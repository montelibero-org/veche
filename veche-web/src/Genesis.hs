{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Genesis where

import Import.NoFoundation

-- global
import Data.Map.Strict qualified as Map

-- project
import Stellar.Horizon.Types (Asset (Asset))
import Stellar.Horizon.Types qualified as Stellar

mtlAsset :: Asset
mtlAsset = Asset "MTL:GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"

mtlFund :: StellarMultiSigAddress
mtlFund =
    StellarMultiSigAddress $
    Stellar.Address "GDX23CPGMQ4LN55VGEDVFZPAJMAUEHSHAMJ2GMCU2ZSHN5QF4TMZYPIS"

forums :: Map ForumId Forum
forums =
    Map.fromList
        [   ( ForumKey "MTL-SIGNERS"
            , Forum{title = "MTL signers", requireUserGroup = Just Signers}
            )
        ,   ( ForumKey "MTL-HOLDERS"
            , Forum{title = "MTL holders", requireUserGroup = Just Holders}
            )
        ,   ( ForumKey "OFFTOPIC"
            , Forum{title = "Offtopic", requireUserGroup = Nothing}
            )
        ,   ( ForumKey "FREELANCE"
            , Forum
                { title             = "Freelance: one-time work offers"
                , requireUserGroup  = Nothing
                }
            )
        ]
