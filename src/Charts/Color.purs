module Charts.Color
       (
         ChartColor(),
         chartColor
       )where

import Prelude ((<<<))
import CSS (Color, toHexString)

newtype ChartColor = ChartColor String

chartColor :: Color -> ChartColor
chartColor = ChartColor <<< toHexString
