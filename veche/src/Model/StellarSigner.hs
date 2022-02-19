{-# LANGUAGE ImportQualifiedPost #-}

module Model.StellarSigner (selectList) where

import Import hiding (selectList)

import Database.Persist qualified as Persist

import Genesis (mtlFund)

selectList :: Handler [Entity StellarSigner]
selectList = runDB $ Persist.selectList [StellarSignerTarget ==. mtlFund] []
