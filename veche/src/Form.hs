{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Form where

import Data.Text (Text)
import Yesod.Core (HandlerSite, MonadHandler, RenderMessage, Route, WidgetFor,
                   getUrlRender, whamlet)
import Yesod.Form (AForm, Enctype (UrlEncoded),
                   Field (Field, fieldEnctype, fieldParse, fieldView),
                   FieldSettings (FieldSettings, fsAttrs, fsName), FormMessage,
                   FormResult, addClass, areq, generateFormPost, parseHelper,
                   runFormPost)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs,
                              renderBootstrap3)

submitButton ::
    (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Text
submitButton label =
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

data SubmitButton = SubmitButton
    {name, value, label :: Text, extraClasses :: [Text]}

submitButtonReq ::
    (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
    SubmitButton -> AForm m Text
submitButtonReq SubmitButton{extraClasses, name, value, label} =
    areq
        (submitButton label)
        fieldSettings
        (Just value)
  where
    fieldSettings0@FieldSettings{fsAttrs} = bfs (mempty :: Text)
    fieldSettings =
        fieldSettings0
            { fsName  = Just name
            , fsAttrs = foldr addClass fsAttrs $ "btn" : extraClasses
            }

data BForm m a = BForm
    { action        :: Maybe (Route (HandlerSite m))
    , extraClasses  :: [Text]
    , aform         :: AForm m a
    , footer        :: WidgetFor (HandlerSite m) ()
    }

bform :: AForm m a -> BForm m a
bform aform = BForm{aform, action = Nothing, footer = mempty, extraClasses = []}

makeFormWidget ::
    Text ->
    BForm m a ->
    WidgetFor (HandlerSite m) () ->
    Enctype ->
    WidgetFor (HandlerSite m) ()
makeFormWidget method BForm{action, footer, extraClasses} fields enctype = do
    urlRender <- getUrlRender
    let attrs =
            ("method", method)
            :   [("action", urlRender act) | Just act <- [action]]
            ++  foldr addClass [] extraClasses
    [whamlet|
        <form *{attrs} enctype=#{enctype} method=post role=form>
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
