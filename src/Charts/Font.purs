module Charts.Font
       (
         ChartFontStyle(),
         chartFontStyle,
         ChartFontWeight(),
         chartFontWeight
       ) where

import CSS (FontStyle(..), FontWeight(..), Prefixed(..), Value(..))

newtype ChartFontStyle = ChartFontStyle String

chartFontStyle :: FontStyle -> ChartFontStyle
chartFontStyle (FontStyle (Value (Plain "italic"))) = ChartFontStyle "italic"
chartFontStyle (FontStyle (Value (Plain "oblique"))) = ChartFontStyle "oblique"
chartFontStyle _ = ChartFontStyle "normal"

newtype ChartFontWeight = ChartFontWeight String

chartFontWeight :: FontWeight -> ChartFontWeight
chartFontWeight (FontWeight (Value (Plain "bold"))) = ChartFontWeight "bold"
chartFontWeight (FontWeight (Value (Plain "bolder"))) = ChartFontWeight "bolder"
chartFontWeight (FontWeight (Value (Plain "lighter"))) = ChartFontWeight "lighter"
chartFontWeight  _ = ChartFontWeight "normal"
