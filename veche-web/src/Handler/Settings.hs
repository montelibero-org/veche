module Handler.Settings (postSettingsLanguageR) where

-- prelude
import Import

-- global
import Yesod.Core (redirectUltDest, setLanguage, setUltDestReferer)

postSettingsLanguageR :: Text -> Handler Void
postSettingsLanguageR lang = do
    setLanguage lang
    setUltDestReferer
    redirectUltDest AboutR
