module Gargantext.Utils.Math where

import Prelude
import Math as Math

roundToMultiple :: Number -> Number -> Number
roundToMultiple eps num = eps * Math.round (num / eps)

-- | Logarithm with given base
logb :: Number -> Number -> Number
logb base n = (Math.log n) / (Math.log base)

log10 :: Number -> Number
log10 = logb 10.0
