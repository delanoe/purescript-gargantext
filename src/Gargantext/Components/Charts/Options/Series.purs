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
                 | Tree | Radial | TreeMap
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
  show Tree     = "tree"               -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=tree-radial
  show Sankey   = "sankey"
  show TreeMap  = "treemap"
  show Scatter  = "scatter"            -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-simple
  show Sunburst = "sunburst"
  show _        = "not implemented yet: should throw error here"

seriesType :: SeriesShape -> SeriesType
seriesType = SeriesType <<< show


type Series = {}
data Serie = SeriesD1 D1 | SeriesD2 D2 | SerieSankey Sankey | SerieTreeMap TreeMap | SerieTree Tree

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
toSeries (SerieTree    a) = unsafeCoerce a

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

mkTreeMap :: Array TreeData -> Serie
mkTreeMap ts = SerieTreeMap { "type" : seriesType TreeMap
                            , "data" : map toTree ts
                          }

type TreeMap = { "type" :: SeriesType
               , "data" :: Array TreeData
               }

data TreeData = TreeLeaf TreeLeaf
              | TreeNode TreeNode

toTree :: TreeData -> TreeData
toTree (TreeLeaf x) = unsafeCoerce x
toTree (TreeNode x) = unsafeCoerce { name : x.name
                                   , value : x.value
                                   , children : (map toTree x.children)
                                   }


type TreeNode =  { name     :: String
                 , value    :: Number
                 , children :: Array TreeData
                 }

type TreeLeaf = { name :: String
                , value :: Number
                }

treeNode :: String -> Number -> Array TreeData -> TreeData
treeNode n v ts = TreeNode {name : n, value:v, children:ts}

treeLeaf :: String -> Number -> TreeData
treeLeaf n v = TreeLeaf { name : n, value : v}


-- | Tree
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=tree-radial

type Tree = { "type" :: SeriesType
            , "data" :: Array TreeData
            , layout :: String
            }

mkTree :: Array TreeData -> Serie
mkTree ts = SerieTree { "type" : seriesType Tree
                 , "data" : map toTree ts
                 , layout : "radial"
                 }


-- | TODO
-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/life-expectancy-table.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter3D-dataset&gl=1

