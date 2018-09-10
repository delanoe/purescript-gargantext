module Gargantext.Components.Charts.Options.Series where

import Effect.Exception (error, Error(..))
import Unsafe.Coerce (unsafeCoerce)
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
  show Bar      = "bar"
  show EffectScatter = "effectScatter" -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-effect
  show Funnel   = "funnel"
  show Heatmap  = "heatmap"
  show Line     = "line"
  show Pie      = "pie"
  show Sankey   = "sankey"
  show Scatter  = "scatter" -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-simple
  show Sunburst = "sunburst"
  show _        = "not implemented yet: should throw error here"

seriesType :: SeriesShape -> SeriesType
seriesType = SeriesType <<< show


type Series = {}
data Serie = SeriesD1 D1 | SeriesD2 D2 | SeriesSankey Sankey

type D1 =
  { name   :: String
  , "type" :: SeriesType
  , "data" :: Array DataS
  }

-- | Scatter Dimension 2 data
type D2 =
  { "symbolSize" :: Number
  , "data" :: Array (Array Number)
  , "type" :: SeriesType
  }

toSeries :: Serie -> Series
toSeries (SeriesD1 a)     = unsafeCoerce a
toSeries (SeriesD2 a)     = unsafeCoerce a
toSeries (SeriesSankey a) = unsafeCoerce a

-- | Sankey Chart
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=sankey-simple
type Sankey = { "type" :: SeriesType
              , layout :: String
              , "data" :: Array Node
              , "links" :: Array Link
              }

type Node = { name :: String}
type Link = { source :: String
            , target :: String
            , value  :: Number
            }

mkSankey :: Array Node -> Array Link -> Serie
mkSankey ns ls = SeriesSankey {"type"   : seriesType Sankey
                              , layout  : "none"
                              , "data"  : ns
                              , "links" : ls
                              }


--
--https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-simple
--
--option = {
--    series: [{
--        type: 'treemap',
--        data: [{
--            name: 'nodeA',            // First tree
--            value: 10,
--            children: [{
--                name: 'nodeAa',       // First leaf of first tree
--                value: 4
--            }, {
--                name: 'nodeAb',       // Second leaf of first tree
--                value: 6
--            }]
--        }, {
--            name: 'nodeB',            // Second tree
--            value: 20,
--            children: [{
--                name: 'nodeBa',       // Son of first tree
--                value: 20,
--                children: [{
--                    name: 'nodeBa1',  // Granson of first tree
--                    value: 20
--                }]
--            }]
--        }]
--    }]
--};
--
--


-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/flare.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=tree-radial


-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/life-expectancy-table.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter3D-dataset&gl=1


