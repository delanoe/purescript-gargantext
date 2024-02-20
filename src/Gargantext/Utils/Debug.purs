module Gargantext.Utils.Debug where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Gargantext.Prelude

foreign import _debugger :: EffectFn1 Unit Unit

debugger :: Unit -> Effect Unit
debugger = runEffectFn1 _debugger
