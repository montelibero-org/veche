{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Form where

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Yesod.Core (HandlerFor, HandlerSite, MonadHandler, RenderMessage, Route,
                   WidgetFor, getUrlRender, whamlet)
import Yesod.Form (AForm, Enctype, FieldSettings (FieldSettings), FormMessage,
                   FormRender, FormResult, addClass, areq, fsAttrs, fsName,
                   generateFormPost, renderDivsNoLabels, runFormPostNoToken,
                   textField)
import Yesod.Form qualified
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (BootstrapHorizontalForm),
                              BootstrapGridOptions (ColSm), renderBootstrap5)

-- | Bootstrap-friendly wrapper around 'AForm'
data BForm m a = BForm
    { action            :: Maybe (Route (HandlerSite m))
    , aform             :: AForm m a
    , classes           :: [Text]
    , header, footer    :: WidgetFor (HandlerSite m) ()
    }

bform :: AForm m a -> BForm m a
bform aform =
    BForm
        { action    = Nothing
        , aform
        , classes   = []
        , footer    = mempty
        , header    = mempty
        }

makeFormWidget ::
    Text ->
    BForm m a ->
    WidgetFor (HandlerSite m) () ->
    Enctype ->
    WidgetFor (HandlerSite m) ()
makeFormWidget method BForm{action, header, footer, classes} fields enctype = do
    urlRender <- getUrlRender
    let attrs =
            [("method", method), ("role", "form"), ("class", "form-horizontal")]
            ++  [("action", urlRender act) | Just act <- [action]]
            ++  foldr addClass [] classes
    [whamlet|
        <form *{attrs} enctype=#{enctype}>
            ^{header}
            ^{fields}
            ^{footer}
    |]

generateFormPostB ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    BForm m a -> m (WidgetFor (HandlerSite m) ())
generateFormPostB b@BForm{aform} = do
    (fields, enctype) <- generateFormPost $ renderForm aform
    pure $ makeFormWidget "post" b fields enctype

runFormPostB ::
    (MonadHandler m) =>
    BForm m a -> m (FormResult a, WidgetFor (HandlerSite m) ())
runFormPostB b@BForm{aform} = do
    ((result, fields), enctype) <- runFormPostNoToken $ renderForm aform
    pure (result, makeFormWidget "post" b fields enctype)

renderForm :: Monad m => FormRender m a
renderForm =
    renderBootstrap5 $
    BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)

actionForm ::
    (HasCallStack, MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    BForm m Text
actionForm =
    -- We don't use "input form", because we need to check the CSRF token
    bform $
    areq
        textField
        FieldSettings
            { fsName    = Just "action"
            , fsId      = Nothing
            , fsAttrs   = undef
            , fsLabel   = undef
            , fsTooltip = undef
            }
        undef
  where
    undef :: HasCallStack => a
    undef = error "actionForm must be user with runFormPostB only"

getPostAction ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    m (FormResult Text)
getPostAction = fst <$> runFormPostB actionForm

generateCsrfField ::
    RenderMessage site FormMessage => HandlerFor site (WidgetFor site ())
generateCsrfField = fmap fst $ generateFormPost $ renderDivsNoLabels $ pure ()

-- | Make button that creates and sends a POST form
actionButton ::
    Route site ->
    [Text] ->   -- ^ additional classes, e.g. @["btn-ganger"]@
    Text ->     -- ^ button label
    Bool ->     -- ^ is enabled
    WidgetFor site ()
actionButton route classes label isEnabled =
    [whamlet|
        <button *{attrs} onclick="submitPostForm('@{route}')"
                :not isEnabled:disabled>
            #{label}
    |]
  where
    attrs = [("class" :: Text, Text.unwords $ "btn" : classes)]

addAttr :: Text -> Text -> FieldSettings site -> FieldSettings site
addAttr name value fs@FieldSettings{fsAttrs} = fs{fsAttrs = fsAttrs'} where
    fsAttrs' = (name, value) : fsAttrs
