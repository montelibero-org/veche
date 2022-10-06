module Templates.Navbar where

import Import.NoFoundation

data MenuItem app =
    MenuItem
        { label     :: Text
        , route     :: Route app
        }
