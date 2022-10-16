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
        [   ( ForumKey "MTL-HOLDERS"
            , Forum
                { title         = "MTL holders"
                , requireRole   = Just MtlHolder
                , allowPoll     = True
                }
            )
        ,   ( ForumKey "OFFTOPIC"
            , Forum
                {title = "Offtopic", requireRole = Nothing, allowPoll = False}
            )
        ,   ( ForumKey "FREELANCE"
            , Forum
                { title         = "Freelance: one-time work offers"
                , requireRole   = Nothing
                , allowPoll     = False
                }
            )
        ]
