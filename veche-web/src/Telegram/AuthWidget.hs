{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.AuthWidget (AuthWidgetResponse (..), authWidgetResponse) where

-- prelude
import Import.NoFoundation

-- global
import Yesod.Core (HandlerSite, RenderMessage)
import Yesod.Form (FormInput, FormMessage)

data AuthWidgetResponse = AuthWidgetResponse
    { id        :: Int64
    , username  :: Text
    }

authWidgetResponse ::
    (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
    FormInput m AuthWidgetResponse
authWidgetResponse = do
    id          <- ireq intField    "id"
    username    <- ireq textField   "username"
    pure AuthWidgetResponse{..}
