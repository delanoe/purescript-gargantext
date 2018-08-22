module Gargantext.Components.Modals.Modal where

import Prelude (Unit)

import Effect (Effect)

foreign import modalShow :: String -> Effect Unit

foreign import modalHide :: String -> Effect Unit
