module Templates.Navbar where

import Foundation.Base

data MenuItem app =
    MenuItem
        { label     :: AppMessage
        , route     :: Route App
        }
