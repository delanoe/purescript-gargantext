module Modal where

import Control.Monad.Eff (Eff)
import Prelude (Unit)

foreign import modalShow :: forall eff. String -> Eff eff Unit

foreign import modalHide :: forall eff. String -> Eff eff Unit
