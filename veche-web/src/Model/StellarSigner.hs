{-# LANGUAGE ImportQualifiedPost #-}

module Model.StellarSigner (
    getByAddress403,
    selectList,
) where

import Import hiding (selectList)

import Database.Persist qualified as Persist

import Genesis (mtlFund)

getByAddress403 :: Text -> Handler (Entity StellarSigner)
getByAddress403 address = runDB $ getBy403 $ UniqueMember mtlFund address

selectList :: Handler [Entity StellarSigner]
selectList = runDB $ Persist.selectList [StellarSignerTarget ==. mtlFund] []
