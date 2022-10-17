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
                { enableContacts    = False
                , enablePoll        = True
                , enablePriceOffer  = False
                , requireRole       = Just MtlHolder
                , title             = "MTL holders"
                }
            )
        ,   ( ForumKey "OFFTOPIC"
            , Forum
                { enableContacts    = False
                , enablePoll        = False
                , enablePriceOffer  = False
                , requireRole       = Nothing
                , title             = "Offtopic"
                }
            )
        ,   ( ForumKey "FREELANCE"
            , Forum
                { enableContacts    = True
                , enablePoll        = False
                , enablePriceOffer  = True
                , requireRole       = Nothing
                , title             = "Freelance: one-time work offers"
                }
            )
        ]
