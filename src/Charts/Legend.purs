module Charts.Legend
       (
         LegendType(..),
         PlainOrScroll(..),
         legendType,
         Orient(),
         Orientation(..),
         orient,
         SelectedMode(),
         LegendMode(..),
         selectedMode
       ) where

import Data.Generic (class Generic, gShow)
import Data.String (toLower)
import Prelude (class Show, show, (<<<))
import Unsafe.Coerce (unsafeCoerce)

newtype LegendType = LegendType String

data PlainOrScroll = Plain | Scroll
instance showPlainOrScroll :: Show PlainOrScroll where
  show (Plain) = "plain"
  show (Scroll) = "scroll"

legendType :: PlainOrScroll -> LegendType
legendType = LegendType <<< toLower <<< show


newtype Orient = Orient String

data Orientation = Horizontal | Vertical
derive instance genericOrientation :: Generic Orientation

orient :: Orientation -> Orient
orient = Orient <<< toLower <<< gShow


foreign import data SelectedMode :: Type

data LegendMode = Bool Boolean | Single | Multiple
derive instance genericLegendMode :: Generic LegendMode

selectedMode :: LegendMode -> SelectedMode
selectedMode (Bool b) = unsafeCoerce b
selectedMode (Single) = unsafeCoerce "single"
selectedMode (Multiple) = unsafeCoerce "multiple"

