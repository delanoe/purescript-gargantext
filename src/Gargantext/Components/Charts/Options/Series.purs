module Gargantext.Components.Charts.Options.Series where

import Unsafe.Coerce (unsafeCoerce)
import Prelude

import Gargantext.Types (class Optional)
import Gargantext.Components.Charts.Options.Color (Color)
import Gargantext.Components.Charts.Options.Data (DataS)


data ItemStyle

type ItemStyleOptional =
  ( color :: Color
  )

itemStyle :: forall o. Optional o ItemStyleOptional => Record o -> ItemStyle
itemStyle = unsafeCoerce

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


data Series

unsafeSeries :: forall o. { | o } -> Series
unsafeSeries = unsafeCoerce

type RequiredD1 o =
  { "type" :: SeriesType
  , "data" :: Array DataS
  | o
  }

type OptionalD1 =
  ( name   :: String
  -- many more...
  )

seriesD1 :: forall o. Optional o OptionalD1 => RequiredD1 o -> Series
seriesD1 = unsafeSeries

seriesFunnelD1 :: forall o. Optional o OptionalD1 => Record o -> Array DataS -> Series
seriesFunnelD1 o ds = seriesD1 ((unsafeCoerce o :: RequiredD1 o) { "data" = ds, "type" = seriesType Funnel })

seriesBarD1 :: forall o. Optional o OptionalD1 => Record o -> Array DataS -> Series
seriesBarD1 o ds = seriesD1 ((unsafeCoerce o :: RequiredD1 o) { "data" = ds, "type" = seriesType Bar })

seriesPieD1 :: forall o. Optional o OptionalD1 => Record o -> Array DataS -> Series
seriesPieD1 o ds = seriesD1 ((unsafeCoerce o :: RequiredD1 o) { "data" = ds, "type" = seriesType Pie })

type RequiredD2 o =
  { "data" :: Array (Array Number)
  , "type" :: SeriesType
  | o
  }

-- | Scatter Dimension 2 data
type OptionalD2 =
  ( name       :: String
  , symbolSize :: Number
  , itemStyle  :: ItemStyle
  -- many more...
  )

seriesD2 :: forall o. Optional o OptionalD2 => RequiredD2 o -> Series
seriesD2 = unsafeSeries

seriesScatterD2 :: forall o. Optional o OptionalD2 => Record o -> Array (Array Number) -> Series
seriesScatterD2 o ds =
  seriesD2 ((unsafeCoerce o :: RequiredD2 o) { "data" = ds, "type" = seriesType Scatter })

type Node = { name :: String}
type Link = { source :: String
            , target :: String
            , value  :: Number
            }

-- | Sankey Chart
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=sankey-simple
type RequiredSankey o =
  { "data" :: Array Node
  , links  :: Array Link
  | o
  }

type OptionalSankey =
  ( layout :: String
  )

seriesSankey :: forall o. Optional o OptionalSankey => RequiredSankey o -> Series
seriesSankey o = unsafeSeries ((unsafeCoerce o) { "type" = seriesType Sankey })

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

type RequiredTree o =
  { "type" :: SeriesType
  , "data" :: Array TreeData
  | o
  }

type OptionalTree =
  ( layout :: String
  )

seriesTree :: forall o. Optional o OptionalTree => RequiredTree o -> Series
seriesTree = unsafeSeries

mkTree :: Trees -> Array TreeData -> Series
mkTree t ts = seriesTree { "type" : SeriesType (show t)
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

