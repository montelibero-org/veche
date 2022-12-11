{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC (rpc) where

-- prelude
import ClassyPrelude hiding (Handler, id)

-- global
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (Value, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.Function ((&))
import Data.Text.Lazy qualified as LText
import Database.Persist (entityVal, getBy, selectList, (==.))
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Network.Stellar.Keypair qualified as Keypair
import Network.Stellar.Signature (verifyBlob)
import Servant qualified
import Servant.Server (Handler, ServerError)

-- project
import Stellar.Simple qualified as Stellar

-- component
import Api (RpcRequest (..), Signature (Signature))
import Genesis (forums)
import Model (Issue, Unique (UniqueUser), User (User))
import Model qualified
import Model.Forum (Forum (Forum), ForumId)
import Model.Forum qualified as Forum
import Model.User (requireAuthzRoleS)

rpc ::
    ConnectionPool ->
    Stellar.Address ->
    Signature ->
    -- | Expected to be a JSON-encoded 'RpcRequest';
    -- we take it as a blob to verify the signature
    ByteString ->
    Handler Value
rpc pool address signature requestBS = do
    checkSignature address requestBS signature
    request <- Aeson.eitherDecodeStrict requestBS & either badRequest pure
    case request of
        GetForumIssues{id, open} ->
            toJSON <$> getForumIssues pool address id open
        GetForums   -> pure $ toJSON forums
        GetSelf     -> toJSON <$> getUser pool address
  where
    badRequest err =
        throwError Servant.err400{Servant.errBody = encodeUtf8 $ LText.pack err}

runDB :: ConnectionPool -> SqlPersistT IO a -> Handler a
runDB pool action = liftIO $ runSqlPool action pool

checkSignature ::
    MonadError ServerError m =>
    Stellar.Address -> ByteString -> Signature -> m ()
checkSignature address request signatureInput = do
    publicKey <- Keypair.decodePublicKey addressText & maybe badUserAddress pure
    signature <- Base64.decode signatureBase64 & either badSignature' pure
    unless (verifyBlob publicKey request signature) badSignature
  where

    Stellar.Address addressText = address

    Signature signatureBase64Text = signatureInput

    signatureBase64 = encodeUtf8 signatureBase64Text

    badUserAddress =
        throwError Servant.err401{Servant.errBody = "Bad user address"}

    badSignature = throwError Servant.err401{Servant.errBody = "Bad signature"}

    badSignature' err =
        throwError
            Servant.err401
            {Servant.errBody = "Bad signature: " <> encodeUtf8 (LText.pack err)}

getUser :: ConnectionPool -> Stellar.Address -> Handler User
getUser pool stellarAddress = do
    muser <- runDB pool $ getBy $ UniqueUser stellarAddress
    pure $ maybe User{name = Nothing, stellarAddress} entityVal muser

getForumIssues ::
    ConnectionPool ->
    Stellar.Address ->
    ForumId ->
    Maybe Bool ->
    Servant.Handler [Issue]
getForumIssues pool stellarAddress forumId mIsOpen = do
    Forum{requireRole} <-
        Forum.get forumId & maybe (throwError Servant.err404) pure
    for_ requireRole $ requireAuthzRoleS pool stellarAddress
    runDB pool $
        selectList [#forum ==. forumId, #open ==. isOpen] [] <&> map entityVal
  where
    isOpen = fromMaybe True mIsOpen
