module Gargantext.Components.Charts.Options.Color
       ( ChartColor()
       , chartColor
       , transparent
       ) where

import Prelude

import CSS (Color, toHexString)
import Color (rgba)

newtype ChartColor = ChartColor String

transparent :: Color
transparent = rgba 255 255 255 0.0

chartColor :: Color -> ChartColor
chartColor = ChartColor <<< toHexString
