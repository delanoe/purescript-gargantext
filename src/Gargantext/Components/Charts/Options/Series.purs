module Gargantext.Components.Charts.Options.Series where

import Effect.Exception (error, Error(..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude

import Gargantext.Components.Charts.Options.Data (DataS)


newtype SeriesType = SeriesType String

type SeriesName = String


data Chart = Line
           | Bar | PictorialBar
           | Pie
           | Scatter | EffectScatter
           | Radar
           | Trees
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
-- Trees

instance showChart :: Show Chart where
  show Bar      = "bar"
  show EffectScatter = "effectScatter" -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-effect
  show Funnel   = "funnel"
  show Heatmap  = "heatmap"
  show Line     = "line"
  show Pie      = "pie"
  show Sankey   = "sankey"
  show Scatter  = "scatter"            -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-simple
  show Sunburst = "sunburst"
  show _        = "not implemented yet: should throw error here"

seriesType :: Chart -> SeriesType
seriesType = SeriesType <<< show


type Series = {}
data Serie = SeriesD1 D1 | SeriesD2 D2 | SerieSankey Sankey | SerieTree Tree

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
toSeries (SerieSankey a) = unsafeCoerce a
toSeries (SerieTree   a) = unsafeCoerce a

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
mkSankey ns ls = SerieSankey { "type"  : seriesType Sankey
                             , layout  : "none"
                             , "data"  : ns
                             , "links" : ls
                             }

-- | * Trees Chart
-- All these Trees are hierarchical Trees structure (or diagram)
-- https://en.wikipedia.org/wiki/Tree_structure

-- Tree types
data Trees = TreeLine | TreeRadial | TreeMap

instance showTrees :: Show Trees where
  show TreeLine    = "tree"           -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=tree-radial
  show TreeRadial  = "tree"           -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter-simple
  show TreeMap     = "treemap"        -- ^ https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-simple


-- TreeLine is a 1-Dimension horizontal hierchical Tree

-- TreeRadial is 1-Dimension radial (as circle) Tree with no surface meaning
-- https://en.wikipedia.org/wiki/Radial_tree
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=tree-radial

-- TreeMap is a is 2-Dimension Tree with surface meaning
-- TreeMap example implementation:
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-simple

type Tree = { "type" :: SeriesType
            , "data" :: Array TreeData
            , layout :: String
            }

mkTree :: Trees -> Array TreeData -> Serie
mkTree t ts = SerieTree { "type" : SeriesType (show t)
                        , "data" : map toJsTree ts
                        , layout : layout
                        }
              where
                layout = case t of
                           TreeRadial -> "radial"
                           _          -> "none"

-- ** Data Structure of the Trees
data TreeData = TreeLeaf TreeLeaf
              | TreeNode TreeNode

toJsTree :: TreeData -> TreeData
toJsTree (TreeLeaf x) = unsafeCoerce x
toJsTree (TreeNode x) = unsafeCoerce { name : x.name
                                     , value : x.value
                                     , children : (map toJsTree x.children)
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


-- | TODO
-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/life-expectancy-table.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter3D-dataset&gl=1

