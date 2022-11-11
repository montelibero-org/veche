{-# LANGUAGE NamedFieldPuns #-}

module WithCallStack where

import Control.Exception (throw)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import UnliftIO (Exception, MonadIO, MonadUnliftIO,
                 SomeException (SomeException), catchAny, displayException,
                 throwIO)

data WithCallStack = WithCallStack
    { stack     :: CallStack
    , parent    :: SomeException
    }

instance Exception WithCallStack where
    displayException WithCallStack{stack, parent} =
        prettyCallStack stack <> ('\n' : displayException parent)

instance Show WithCallStack where
    show = displayException

addCallStack :: (HasCallStack, MonadUnliftIO m) => m a -> m a
addCallStack action =
    action `catchAny` \e -> throwIO $ WithCallStack callStack e

withCallStack :: (HasCallStack, Exception e) => e -> WithCallStack
withCallStack e = WithCallStack{stack = callStack, parent = SomeException e}

throwWithCallStackIO :: (HasCallStack, Exception e, MonadIO m) => e -> m a
throwWithCallStackIO = throwIO . withCallStack

impureThrowWithCallStack :: (HasCallStack, Exception e) => e -> a
impureThrowWithCallStack = throw . withCallStack
