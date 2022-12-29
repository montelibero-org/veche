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
import Stellar.Simple (Asset (Asset), mkAsset)
import Stellar.Simple qualified as Stellar
import Stellar.Simple.Types (assetToText)

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
        [   ( ForumKey "FCM-HOLDERS"
            , Forum
                { enableAttachTx    = False
                , enableContacts    = False
                , enablePriceOffer  = False
                , pollOptions       = [ByAmountOfFcm]
                , requireRole       = Just HolderOfFcm
                , title             = "FCM holders"
                }
            )
        ,   ( ForumKey "MTL-HOLDERS"
            , Forum
                { enableAttachTx    = False
                , enableContacts    = False
                , enablePriceOffer  = False
                , pollOptions       = [ByMtlAmount, BySignerWeight]
                , requireRole       = Just MtlHolder
                , title             = "MTL holders"
                }
            )
        ,   ( ForumKey "MTL-SIGNERS"
            , Forum
                { enableAttachTx    = True
                , enableContacts    = False
                , enablePriceOffer  = False
                , pollOptions       = [BySignerWeight]
                , requireRole       = Just MtlSigner
                , title             = "MTL signers"
                }
            )
        ,   ( ForumKey "VECHE-HOLDERS"
            , Forum
                { enableAttachTx    = False
                , enableContacts    = False
                , enablePriceOffer  = False
                , pollOptions       = [ByAmountOfVeche]
                , requireRole       = Just HolderOfVeche
                , title             = "VECHE holders"
                }
            )
        ,   ( ForumKey "OFFTOPIC"
            , Forum
                { enableAttachTx    = False
                , enableContacts    = False
                , enablePriceOffer  = False
                , pollOptions       = []
                , requireRole       = Nothing
                , title             = "Offtopic"
                }
            )
        ,   ( ForumKey "FREELANCE"
            , Forum
                { enableAttachTx    = False
                , enableContacts    = True
                , enablePriceOffer  = True
                , pollOptions       = []
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
knownAssets = Set.fromList [mtlAsset, eurmtl, fcmAsset, vecheAsset]

showKnownAsset :: Asset -> Text
showKnownAsset a@Asset{code}
    | a `elem` knownAssets  = code
    | otherwise             = assetToText a

mtlKeyRateAccount :: Stellar.Address
mtlKeyRateAccount =
    Stellar.Address "GDGGHSIA62WGNMN2VOIBW3X66ATOBW5J2FU7CSJZ6XVHI2ZOXZCRRATE"

fcmAsset :: Asset
fcmAsset =
    mkAsset "FCM" "GDIE253MSIYMFUS3VHRGEQPIBG7VAIPSMATWLTBF73UPOLBUH5RV2FCM"

vecheAsset :: Asset
vecheAsset =
    mkAsset "VECHE" "GDUMR6C3XNIMXUCS3WR7DZDWWDWAGRCCNZ23FWXAMKAIYOGKS7KN47AG"
