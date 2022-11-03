{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Genesis where

import Import.NoFoundation

-- global
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- project
import Stellar.Simple (Asset (Asset), assetToText, mkAsset)
import Stellar.Simple qualified as Stellar

mtlIssuer :: Text
mtlIssuer = "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"

mtlAsset :: Asset
mtlAsset = mkAsset "MTL" mtlIssuer

eurmtl :: Asset
eurmtl = mkAsset "EURMTL" mtlIssuer

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

escrowAddress :: Stellar.Address
escrowAddress =
    Stellar.Address "GAS5XNXJJPOOJ73ODLCHGMEY4PUZB5S2TIXUBSYYMCNIYL6PHZGCB7RW"

escrowFederatedHost :: Text
escrowFederatedHost = "veche.montelibero.org"

knownAssets :: Set Asset
knownAssets = Set.fromList [mtlAsset, eurmtl]

showKnownAsset :: Asset -> Text
showKnownAsset a@Asset{code}
    | a `elem` knownAssets  = code
    | otherwise             = assetToText a
