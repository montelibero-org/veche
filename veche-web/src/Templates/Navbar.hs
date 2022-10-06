module Templates.Navbar where

import Foundation.Base
import Import.NoFoundation

data MenuItem = MenuItem{label :: Text, route :: Route App}
