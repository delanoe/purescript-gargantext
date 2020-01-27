module Gargantext.Utils.HighlightJS where

import DOM.Simple (Element)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

import Gargantext.Prelude

highlightBlock :: Element -> Effect Unit
highlightBlock el = runEffectFn1 _highlightBlock el

foreign import _highlightBlock :: EffectFn1 Element Unit
