module Model.StellarSigner (
    getByAddress403,
    selectAll,
) where

import Import

import Genesis (mtlFund)

getByAddress403 :: Text -> Handler (Entity StellarSigner)
getByAddress403 address = runDB $ getBy403 $ UniqueMember mtlFund address

selectAll :: MonadIO m => SqlPersistT m [Entity StellarSigner]
selectAll = selectList [StellarSignerTarget ==. mtlFund] []
