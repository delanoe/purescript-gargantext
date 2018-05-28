module Charts.Color where

import Prelude ((<<<))
import CSS (Color, toHexString)

newtype ChartColor = ChartColor String

renderChartColor :: Color -> ChartColor
renderChartColor = ChartColor <<< toHexString
