module Gargantext.Utils.Popover where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import DOM.Simple as DOM
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import FFI.Simple ((..), (...))
import Reactix as R

import Gargantext.Prelude

type PopoverRef = R.Ref (Nullable DOM.Element)

type Props =
  (
    open :: Boolean
  , onClose :: Unit -> Effect Unit
  , onOpen :: Unit -> Effect Unit
  , ref :: PopoverRef
  )

foreign import popoverCpt :: R.Component Props

popover :: Record Props -> Array R.Element -> R.Element
popover = R.rawCreateElement popoverCpt

foreign import _setState :: EffectFn2 DOM.Element Boolean Unit

setState :: DOM.Element -> Boolean -> Effect Unit
setState = runEffectFn2 _setState

setOpen :: PopoverRef -> Boolean -> Effect Unit
setOpen ref val =
  case toMaybe $ R.readRef ref of
    Nothing -> pure unit
    Just p  -> do
      setState p val
