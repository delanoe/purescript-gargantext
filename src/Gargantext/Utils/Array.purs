module Gargantext.Utils.Array (push) where

import Effect (Effect)
import Data.Unit (Unit)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import _push :: forall a. EffectFn2 (Array a) a Unit

push :: forall a. Array a -> a -> Effect Unit
push = runEffectFn2 _push
