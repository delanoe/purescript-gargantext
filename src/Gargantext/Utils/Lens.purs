module Gargantext.Utils.Lens where

import Gargantext.Prelude

import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Lens
import Data.Traversable

-- | Given a Traversable of entities and a lens for them, normalize
-- | over lens getter so that the value of lens setter is in range [0,
-- | 1].
normalizeLens :: forall a t. Traversable t => Lens' a Number -> t a -> t a
normalizeLens l ns = over traversed normalize' ns
  where
    values = over traversed (_ ^. l) ns
    vMin = minimum values
    vMax = maximum values
    vRange = do
      minv <- vMin
      maxv <- vMax
      pure $ maxv - minv
    divisor = maybe 1.0 (\r -> 1.0 / r) vRange
    min = fromMaybe 0.0 vMin
    normalize' n = over l (\v -> (v - min) * divisor) n
