module Gargantext.Utils.Selection
  ( class ToString, Selection, toString, getSelection ) where

import Prelude ((<$>))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | Represents a text selection
foreign import data Selection :: Type
-- foreign import data Range :: Type -- Probably coming soon

-- toString

class ToString t

instance toStringSelection :: ToString Selection
-- instance toStringRange :: ToString Range

foreign import _toString :: forall t. t -> String

-- | Renders a selection or range as a string
toString :: forall t. ToString t => t -> String
toString = _toString

-- getSelection

foreign import _getSelection :: Effect (Nullable Selection)

-- | Fetches the current text selection, if any
getSelection :: Effect (Maybe Selection)
getSelection = toMaybe <$> _getSelection

