module Gargantext.Utils.Range where

import Data.Hashable (class Hashable, hash)
import Data.Newtype (class Newtype)
import Prelude hiding (clamp)

class Range r v where
  clamp :: r -> v -> v
  within :: r -> v -> Boolean

-- | A Closed Interval, in math speak
newtype Closed t = Closed { min :: t, max :: t }
derive instance Newtype (Closed t) _
instance Ord t => Range (Closed t) t where
  clamp (Closed r) = max r.min <<< min r.max
  within (Closed r) v = (r.min <= v) && (v <= r.max)
instance Eq t => Eq (Closed t) where
  eq (Closed r1) (Closed r2) = (r1.min == r2.min) && (r1.max == r2.max)
instance Hashable t => Hashable (Closed t) where
  hash (Closed { min, max }) = hash { min, max }

type NumberRange = Closed Number

range :: NumberRange -> Number
range (Closed r) = r.max - r.min

-- | Clamps the value to within the range and returns a normalised
-- | (0-1) float indication progress along the range
normalise :: NumberRange -> Number -> Number
normalise r@(Closed {min}) v = (clamp r v - min) / range r

-- | Given a normal (0-1) float representing progress along a range,
-- | project it onto the range
projectNormal :: NumberRange -> Number -> Number
projectNormal r@(Closed {min}) v = (clamp closedProbability v * range r) + min

-- | A closed range between 0 and 1
closedProbability :: NumberRange
closedProbability = Closed { min: 0.0, max: 1.0 }

-- | Updates the minimum value in a closed range
withMin :: forall t. Closed t -> t -> Closed t
withMin (Closed {max}) min = Closed { min, max }

-- | Updates the maximum value in a closed range
withMax :: forall t. Closed t -> t -> Closed t
withMax (Closed {min}) max = Closed { min, max }
