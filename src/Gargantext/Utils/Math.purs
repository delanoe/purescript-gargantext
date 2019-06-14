module Gargantext.Utils.Math where

import Prelude
import Math as Math

roundToMultiple :: Number -> Number -> Number
roundToMultiple num eps = eps * Math.round (num / eps)

