{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Verifier (
    -- * Scenario for a known address
    getKey,
    checkAndRemoveKey,
    -- * Scenario without address known
    getKeyNoAddress,
    getAndRemoveKey,
) where

-- prelude
import Foundation.Base
import Import.NoFoundation

-- global
import Crypto.Nonce (nonce128urlT)
import Crypto.Nonce qualified
import Data.Time (addUTCTime, secondsToNominalDiffTime)
import Database.Persist (delete, getBy, insert_)
import Stellar.Horizon.Client qualified as Stellar
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Core (deleteSession, liftHandler, lookupSession, setSession)
import Yesod.Persist (runDB)

-- component
import Model (Unique (UniqueVerifier), Verifier (Verifier))
import Model qualified

nonceGenerator :: Crypto.Nonce.Generator
nonceGenerator = unsafePerformIO Crypto.Nonce.new
{-# NOINLINE nonceGenerator #-}

sessionNonceKey :: Text
sessionNonceKey = "auth.verifier.nonce"

setSessionNonce :: MonadHandler m => Text -> m ()
setSessionNonce = setSession sessionNonceKey

lookupSessionNonce :: MonadHandler m => m (Maybe Text)
lookupSessionNonce = lookupSession sessionNonceKey

deleteSessionNonce :: MonadHandler m => m ()
deleteSessionNonce = deleteSession sessionNonceKey

getKeyNoAddress :: Handler Text
getKeyNoAddress = do
    mSessionNonce <- lookupSessionNonce
    case mSessionNonce of
        Just nonce -> pure nonce
        _ -> do
            nonce <- nonce128urlT nonceGenerator
            setSessionNonce nonce
            pure nonce

getAndRemoveKey :: Handler (Maybe Text)
getAndRemoveKey = do
    mSessionNonce <- lookupSessionNonce
    for_ mSessionNonce $ const deleteSessionNonce
    pure mSessionNonce

-- * Using database

getKey :: Stellar.Address -> Handler Text
getKey userIdent =
    liftHandler . runDB $ do
        mSessionNonce <- lookupSessionNonce
        mSavedVerifier <-
            case mSessionNonce of
                Nothing     -> pure Nothing
                Just nonce  -> getBy $ UniqueVerifier userIdent nonce
        now <- liftIO getCurrentTime
        case mSavedVerifier of
            Just (Entity _ Verifier{key, expires})
                | expires > addUTCTime minTtl now -> pure key
            _ -> do
                nonce <- nonce128urlT nonceGenerator
                let expires = addUTCTime maxTtl now
                insert_ Verifier{key = nonce, expires, userIdent}
                setSessionNonce nonce
                pure nonce
  where
    minTtl = secondsToNominalDiffTime   60      --  1 minute
    maxTtl = secondsToNominalDiffTime $ 60 * 15 -- 15 minutes

checkAndRemoveKey :: Stellar.Address -> Text -> Handler Bool
checkAndRemoveKey verifierUserIdent verifierKey =
    runDB do
        mVerifier <- getBy $ UniqueVerifier verifierUserIdent verifierKey
        case mVerifier of
            Just (Entity verifierId Verifier{expires}) -> do
                delete verifierId
                deleteSessionNonce
                now <- liftIO getCurrentTime
                pure $ now <= expires
            Nothing -> pure False
