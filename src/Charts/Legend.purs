module Charts.Legend
       (
         LegendType(),
         PlainOrScroll(..),
         legendType,
         Orient(),
         Orientation(..),
         orient
       ) where

import Data.Generic (class Generic, gShow)
import Data.String (toLower)
import Prelude ((<<<))

newtype LegendType = LegendType String

data PlainOrScroll = Plain | Scroll
derive instance genericPlainOrScroll :: Generic PlainOrScroll

legendType :: PlainOrScroll -> LegendType
legendType = LegendType <<< toLower <<< gShow


newtype Orient = Orient String

data Orientation = Horizontal | Vertical
derive instance genericOrientation :: Generic Orientation

orient :: Orientation -> Orient
orient = Orient <<< toLower <<< gShow
