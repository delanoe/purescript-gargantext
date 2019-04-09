module Gargantext.Utils.React where

import Prelude
import Data.Array ((!!))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (fromMaybe)
import React (ReactElement, Children)

-- TODO: Upgrade thermite and reapply our changes or upstream them and get rid of this
type WithChildren props = { children :: Children | props }

wrap :: (Array ReactElement -> ReactElement) -> ReactElement -> ReactElement
wrap f e = f [e]

