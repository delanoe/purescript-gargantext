module Gargantext.Utils.Math where

import Data.Number as DN
import Prelude

roundToMultiple :: Number -> Number -> Number
roundToMultiple eps num = eps * DN.round (num / eps)

-- | Logarithm with given base
logb :: Number -> Number -> Number
logb base n = (DN.log n) / (DN.log base)

log10 :: Number -> Number
log10 = logb 10.0
