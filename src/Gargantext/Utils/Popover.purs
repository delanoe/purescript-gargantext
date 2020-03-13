module Gargantext.Utils.Popover where

import Effect (Effect)
import Reactix as R

import Gargantext.Prelude

type Props =
  (
    open :: Boolean
  , onClose :: Unit -> Effect Unit
  , onOpen :: Unit -> Effect Unit
  )

foreign import popoverCpt :: R.Component Props

popover :: Record Props -> Array R.Element -> R.Element
popover = R.rawCreateElement popoverCpt
