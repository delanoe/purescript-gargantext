module Gargantext.Utils.Math where

import Prelude
import Math as Math

roundToMultiple :: Number -> Number -> Number
roundToMultiple eps num = eps * Math.round (num / eps)
