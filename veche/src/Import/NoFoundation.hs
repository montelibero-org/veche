module Import.NoFoundation
    ( module Import
    , inflect
    ) where

import ClassyPrelude.Yesod as Import
import Data.Function as Import ((&))
import Data.Kind as Import (Type)
import Yesod.Auth as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Default.Config2 as Import

import Model as Import
import Settings as Import
import Settings.StaticFiles as Import

inflect :: Int -> String -> String -> String
inflect 1 single _ = "1 " <> single
inflect n _ plural = show n <> " " <> plural
