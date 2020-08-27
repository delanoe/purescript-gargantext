module Gargantext.Utils.ReactTooltip where

import Data.Maybe (maybe)
import Data.Nullable (Nullable, toMaybe)
import DOM.Simple as DOM
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Reactix as R

import Gargantext.Prelude

type Props =
  (
    id :: String
  )

foreign import reactTooltipCpt :: R.Component Props

reactTooltip :: Record Props -> Array R.Element -> R.Element
reactTooltip = R.rawCreateElement reactTooltipCpt
