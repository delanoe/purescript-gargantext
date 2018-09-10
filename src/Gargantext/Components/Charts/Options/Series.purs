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
  show TreeMap  = "treemap"
  show Scatter  = "scatter" -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-simple
  show Sunburst = "sunburst"
  show _        = "not implemented yet: should throw error here"

seriesType :: SeriesShape -> SeriesType
seriesType = SeriesType <<< show


type Series = {}
data Serie = SeriesD1 D1 | SeriesD2 D2 | SerieSankey Sankey | SerieTreeMap TreeMap

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
toSeries (SeriesD1 a)    = unsafeCoerce a
toSeries (SeriesD2 a)    = unsafeCoerce a
toSeries (SerieSankey  a) = unsafeCoerce a
toSeries (SerieTreeMap a) = unsafeCoerce a

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
mkSankey ns ls = SerieSankey {"type"   : seriesType Sankey
                              , layout  : "none"
                              , "data"  : ns
                              , "links" : ls
                              }

-- | TreeMap Chart
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-simple

mkTreeMap :: Array TreeMapTree -> Serie
mkTreeMap ts = SerieTreeMap { "type" : seriesType TreeMap
                            , "data" : map toTreeMap ts
                          }

type TreeMap = { "type" :: SeriesType
               , "data" :: Array TreeMapTree
               }

data TreeMapTree = TreeMapLeaf TreeMapLeaf
                 | TreeMapNode TreeMapNode

toTreeMap :: TreeMapTree -> TreeMapTree
toTreeMap (TreeMapLeaf x) = unsafeCoerce x
toTreeMap (TreeMapNode x) = unsafeCoerce { name : x.name
                                         , value : x.value
                                         , children : (map toTreeMap x.children)
                                         }


type TreeMapNode =  { name     :: String
                    , value    :: Number
                    , children :: Array TreeMapTree
                    }

type TreeMapLeaf = { name :: String
                   , value :: Number
                   }

treeMapNode :: String -> Number -> Array TreeMapTree -> TreeMapTree
treeMapNode n v ts = TreeMapNode {name : n, value:v, children:ts}

treeMapLeaf :: String -> Number -> TreeMapTree
treeMapLeaf n v = TreeMapLeaf { name : n, value : v}


-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/flare.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=tree-radial


-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/life-expectancy-table.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter3D-dataset&gl=1


