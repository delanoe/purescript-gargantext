module Gargantext.Components.Modals.Modal where

import Prelude (Unit)

import Control.Monad.Eff (Eff)

foreign import modalShow :: forall eff. String -> Eff eff Unit

foreign import modalHide :: forall eff. String -> Eff eff Unit
