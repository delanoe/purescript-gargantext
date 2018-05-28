module Charts.Font where

import CSS (FontStyle(..), FontWeight(..), Prefixed(..), Value(..))

newtype ChartFontStyle = ChartFontStyle String

renderChartFontStyle :: FontStyle -> ChartFontStyle
renderChartFontStyle (FontStyle (Value (Plain "italic"))) = ChartFontStyle "italic"
renderChartFontStyle (FontStyle (Value (Plain "oblique"))) = ChartFontStyle "oblique"
renderChartFontStyle _ = ChartFontStyle "normal"

newtype ChartFontWeight = ChartFontWeight String

renderChartFontWeight :: FontWeight -> ChartFontWeight
renderChartFontWeight (FontWeight (Value (Plain "bold"))) = ChartFontWeight "bold"
renderChartFontWeight (FontWeight (Value (Plain "bolder"))) = ChartFontWeight "bolder"
renderChartFontWeight (FontWeight (Value (Plain "lighter"))) = ChartFontWeight "lighter"
renderChartFontWeight  _ = ChartFontWeight "normal"
