{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Yesod.Auth.Stellar.Internal where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)

type TextL = TextL.Text

python3 :: MonadIO m => TextL -> m (Either Text Text)
python3 program = do
    (exitCode, out, err) <-
        liftIO $
        readProcess $
        proc "python3" [] & setStdin (byteStringInput $ encodeUtf8 program)
    pure
        case exitCode of
            ExitSuccess -> Right $ Text.strip $ TextL.toStrict $ decodeUtf8 out
            ExitFailure _ -> Left $ TextL.toStrict $ decodeUtf8 err
