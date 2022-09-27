{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Form where

import Data.Text (Text)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Yesod.Core (HandlerFor, HandlerSite, MonadHandler, RenderMessage, Route,
                   WidgetFor, getUrlRender, whamlet)
import Yesod.Form (AForm, Enctype (UrlEncoded),
                   Field (Field, fieldEnctype, fieldParse, fieldView),
                   FieldSettings (FieldSettings, fsAttrs, fsName), FormMessage,
                   FormResult, addClass, areq, generateFormPost, parseHelper,
                   renderDivsNoLabels, runFormPost, textField)
import Yesod.Form qualified
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

submitField ::
    (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Text
submitField label =
    Field
        { fieldParse = parseHelper Right
        , fieldView = \theId name attrs val isReq ->
            [whamlet|
                $newline never
                <button id=#{theId} name=#{name} type=submit *{attrs}
                        :isReq:required value=#{either id id val}>
                    #{label}
            |]
        , fieldEnctype = UrlEncoded
        }

data Submit = Submit{action, label :: Text, classes :: [Text]}

submitReq ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    Submit -> AForm m Text
submitReq Submit{classes, action, label} =
    areq (submitField label) fieldSettings (Just action)
  where
    fieldSettings0@FieldSettings{fsAttrs} = bfs ("" :: Text)
    fieldSettings =
        fieldSettings0
            { fsName  = Just "action"
            , fsAttrs = foldr addClass fsAttrs $ "btn" : classes
            }

submit ::
    (MonadHandler handler, RenderMessage (HandlerSite handler) FormMessage) =>
    Text -> Text -> [Text] -> AForm handler Void
submit action label classes =
    error "Void" <$ submitReq Submit{action, label, classes}

data BForm m a = BForm
    { action  :: Maybe (Route (HandlerSite m))
    , classes :: [Text]
    , aform   :: AForm m a
    , footer  :: WidgetFor (HandlerSite m) ()
    }

bform :: AForm m a -> BForm m a
bform aform = BForm{aform, action = Nothing, footer = mempty, classes = []}

makeFormWidget ::
    Text ->
    BForm m a ->
    WidgetFor (HandlerSite m) () ->
    Enctype ->
    WidgetFor (HandlerSite m) ()
makeFormWidget method BForm{action, footer, classes} fields enctype = do
    urlRender <- getUrlRender
    let attrs =
            [("method", method), ("role", "form")]
            ++  [("action", urlRender act) | Just act <- [action]]
            ++  foldr addClass [] classes
    [whamlet|
        <form *{attrs} enctype=#{enctype}>
            ^{fields}
            ^{footer}
    |]

generateFormPostB ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    BForm m a -> m (WidgetFor (HandlerSite m) ())
generateFormPostB b@BForm{aform} = do
    (fields, enctype) <-
        generateFormPost $ renderBootstrap3 BootstrapBasicForm aform
    pure $ makeFormWidget "post" b fields enctype

runFormPostB ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    BForm m a -> m (FormResult a, WidgetFor (HandlerSite m) ())
runFormPostB b@BForm{aform} = do
    ((result, fields), enctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm aform
    pure (result, makeFormWidget "post" b fields enctype)

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
