module Gargantext.Utils.Range
  ( class Range, clamp, within
  , Closed(..), closedProbability
  ) where

import Prelude ((<<<), (&&), (<=), (>=), min, max)
import Data.Ord (class Ord)

class Range r v where
  clamp :: r -> v -> v
  within :: r -> v -> Boolean

-- | A Closed Interval, in math speak
newtype Closed t = Closed { min :: t, max :: t }

instance closedRange :: Ord t => Range (Closed t) t where
  clamp (Closed r) = max r.min <<< min r.max
  within (Closed r) v = (v <= r.max) && (v >= r.min)

-- | A closed range between 0 and 1
closedProbability :: Closed Number
closedProbability = Closed { min: 0.0, max: 1.0 }

