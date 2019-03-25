module Gargantext.Utils.Reactil where

import Prelude
import React (ReactElement)

wrap :: (Array ReactElement -> ReactElement) -> ReactElement -> ReactElement
wrap f e = f [e]
