module Gargantext.Components.Charts.Options.Series where

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


type Series = {}
data Series' = SeriesD1 D1 | SeriesD2 D2

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


toSeries :: Series' -> Series
toSeries (SeriesD1 a) = unsafeCoerce a
toSeries (SeriesD2 a) = unsafeCoerce a




-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=sankey-simple
-- type Sankey = { "type" :: SeriesType
--               , "layout" :: String
--               , 
-- option = {
--     series: {
--         type: 'sankey',
--         layout:'none',
--         data: [{
--             name: 'a'
--         }, {
--             name: 'b'
--         }, {
--             name: 'a1'
--         }, {
--             name: 'a2'
--         }, {
--             name: 'b1'
--         }, {
--             name: 'c'
--         }],
--         links: [{
--             source: 'a',
--             target: 'a1',
--             value: 5
--         }, {
--             source: 'a',
--             target: 'a2',
--             value: 3
--         }, {
--             source: 'b',
--             target: 'b1',
--             value: 8
--         }, {
--             source: 'a',
--             target: 'b1',
--             value: 3
--         }, {
--             source: 'b1',
--             target: 'a1',
--             value: 1
--         }, {
--             source: 'b1',
--             target: 'c',
--             value: 2
--         }]
--     }
--   };

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
















































