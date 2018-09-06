module Gargantext.Components.Charts.Options.Series where

import Prelude

import Gargantext.Components.Charts.Options.Data (DataS)

newtype SeriesType = SeriesType String

type SeriesName = String

data SeriesShape = Line
                 | Bar | PictorialBar
                 | Pie
                 | Scatter | EffectScatter
                 | Radar
                 | Tree | TreeMap
                 | Sunburst
                 | Boxplot
                 | Candlestick
                 | Heatmap
                 | Map
                 | Parallel
                 | Lines
                 | Graph
                 | Sankey
                 | Funnel
                 | Gauge
                 | ThemeRiver

instance showSeriesShape :: Show SeriesShape where
  show Line     = "line"
  show Bar      = "bar"
  show Pie      = "pie"
  show Sunburst = "sunburst"
  show Funnel   = "funnel"
  show Heatmap  = "heatmap"
  show EffectScatter = "effectScatter" -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-effect
  show Scatter  = "scatter" -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-simple
  show _        = ""

seriesType :: SeriesShape -> SeriesType
seriesType = SeriesType <<< show


data Series = SeriesD1 D1 | SeriesD2 D2

type D1 =
  { name   :: String
  , "data" :: Array DataS
  , "type" :: SeriesType
  }

type D2 =
  { "symbolSize" :: Number
  , "data" :: Array (Array Number)
  , "type" :: SeriesType
}
