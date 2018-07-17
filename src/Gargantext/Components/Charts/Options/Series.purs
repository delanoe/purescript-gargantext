module Gargantext.Components.Charts.Options.Series where

import Prelude

import Gargantext.Components.Charts.Options.Data (DataS)


newtype SeriesType = SeriesType String

type SeriesName = String

data SeriesShape = Line
                 | Bar | PictorialBar
                 | Pie
                 | Scatter | EffectScater
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
  show _        = ""

seriesType :: SeriesShape -> SeriesType
seriesType = SeriesType <<< show

type Series =
  { name   :: String
  , "type" :: SeriesType
  , "data" :: Array DataS
  }
