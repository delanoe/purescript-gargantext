module Gargantext.Utils.Popover where

import Data.Nullable (Nullable)
import DOM.Simple.Types (Element)
import Reactix as R
import Type.Row as TR

import Gargantext.Prelude

type Props = ()

foreign import popoverCpt :: R.Component Props

popover :: Record Props -> Array R.Element -> R.Element
popover = R.rawCreateElement popoverCpt
