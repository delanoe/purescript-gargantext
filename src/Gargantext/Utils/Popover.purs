module Gargantext.Utils.Popover where

import Data.Maybe (maybe)
import Data.Nullable (Nullable, toMaybe)
import DOM.Simple as DOM
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Reactix as R

import Gargantext.Prelude

type PopoverRef = R.Ref (Nullable DOM.Element)

type Props =
  (
    arrow :: Boolean
  , open :: Boolean
  , onClose :: Unit -> Effect Unit
  , onOpen :: Unit -> Effect Unit
  , ref :: PopoverRef
  )

foreign import popoverCpt :: R.Component Props

popover :: Record Props -> Array R.Element -> R.Element
popover = R.rawCreateElement popoverCpt

foreign import _setState :: forall a. EffectFn2 DOM.Element a Unit

setState :: forall a. DOM.Element -> a -> Effect Unit
setState = runEffectFn2 _setState

setOpen :: PopoverRef -> Boolean -> Effect Unit
setOpen ref val = maybe (pure unit) (\p -> setState p {open: val}) $ toMaybe $ R.readRef ref
