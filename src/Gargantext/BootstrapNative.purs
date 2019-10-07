module Gargantext.BootstrapNative where

import Effect (Effect)
import Gargantext.Prelude

foreign import createDropdown :: String -> Effect Unit
