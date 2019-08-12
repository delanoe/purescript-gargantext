module Gargantext.Utils.Range where

import Prelude
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)

class Range r v where
  clamp :: r -> v -> v
  within :: r -> v -> Boolean

-- | A Closed Interval, in math speak
newtype Closed t = Closed { min :: t, max :: t }

derive instance newtypeClosed :: Newtype (Closed t) _

instance closedRange :: Ord t => Range (Closed t) t where
  clamp (Closed r) = max r.min <<< min r.max
  within (Closed r) v = (v <= r.max) && (v >= r.min)

range :: Closed Number -> Number
range (Closed r) = r.max - r.min

-- | Clamps the value to within the range and returns a normalised
-- | (0-1) float indication progress along the range
normalise :: Closed Number -> Number -> Number
normalise r v = clamp r v / range r

-- | Given a normal (0-1) float representing progress along a range,
-- | project it onto the range
projectNormal :: Closed Number -> Number -> Number
projectNormal r v = clamp closedProbability v * range r

-- | A closed range between 0 and 1
closedProbability :: Closed Number
closedProbability = Closed { min: 0.0, max: 1.0 }

-- | Updates the minimum value in a closed range
withMin :: forall t. Closed t -> t -> Closed t
withMin (Closed {max}) min = Closed { min, max }

-- | Updates the maximum value in a closed range
withMax :: forall t. Closed t -> t -> Closed t
withMax (Closed {min}) max = Closed { min, max }

