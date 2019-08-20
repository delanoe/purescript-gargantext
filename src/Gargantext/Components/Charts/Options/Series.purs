module Gargantext.Components.Charts.Options.Series where

import Data.Maybe
import Data.Array (foldl)
import Record.Unsafe (unsafeSet)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Gargantext.Types (class Optional)
import Gargantext.Components.Charts.Options.Font (ItemStyle, Tooltip)
import Gargantext.Components.Charts.Options.Data (DataD1, DataD2)

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


-- | Scatter Dimension 2 data
type OptionalSeries =
  ( name       :: String
  , symbolSize :: Number
  , itemStyle  :: ItemStyle
    -- ^ Graphic style of, *emphasis* is the style when it is highlighted, like being hovered by mouse, or highlighted via legend connect.
    --   https://ecomfe.github.io/echarts-doc/public/en/option.html#series-scatter.itemStyle
  , tooltip    :: Tooltip

  -- many more...
  )

data Series

unsafeSeries :: forall o. Record o -> Series
unsafeSeries = unsafeCoerce

type RequiredSeriesD1 o =
  { "type" :: SeriesType
  , "data" :: Array DataD1
  | o
  }

seriesD1 :: forall o. Optional o OptionalSeries => RequiredSeriesD1 o -> Series
seriesD1 = unsafeSeries

seriesFunnelD1 :: forall o. Optional o OptionalSeries => Record o -> Array DataD1 -> Series
seriesFunnelD1 o ds = unsafeSeries (unsafeSet "data" ds (unsafeSet "type" (seriesType Funnel) o))

seriesBarD1 :: forall o. Optional o OptionalSeries => Record o -> Array DataD1 -> Series
seriesBarD1 o ds = unsafeSeries (unsafeSet "data" ds (unsafeSet "type" (seriesType Bar) o))

seriesPieD1 :: forall o. Optional o OptionalSeries => Record o -> Array DataD1 -> Series
seriesPieD1 o ds = unsafeSeries (unsafeSet "data" ds (unsafeSet "type" (seriesType Pie) o))

type RequiredSeriesD2 o =
  { "data" :: Array DataD2
  , "type" :: SeriesType
  | o
  }

seriesD2 :: forall o. Optional o OptionalSeries => RequiredSeriesD2 o -> Series
seriesD2 = unsafeSeries

seriesScatterD2 :: forall o. Optional o OptionalSeries => Record o -> Array DataD2 -> Series
seriesScatterD2 o ds =
  unsafeCoerce (unsafeSet "data" ds (unsafeSet "type" (seriesType Scatter) o))

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
  , "data" :: Array TreeNode
  | o
  }

type OptionalTree =
  ( layout :: String
  )

seriesTree :: forall o. Optional o OptionalTree => RequiredTree o -> Series
seriesTree = unsafeSeries

mkTree :: Trees -> Array TreeNode -> Series
mkTree t ts = seriesTree { "type" : SeriesType (show t)
                         , "data" : map (toJsTree Nothing) ts
                         , layout : layout
                         }
              where
                layout = case t of
                           TreeRadial -> "radial"
                           _          -> "none"

-- ** Data Structure of the Trees
data TreeData = Array TreeNode


treeValue :: TreeNode -> Int
treeValue (TreeNode x) = foldl (+) 0 $ [x.value] <> map treeValue x.children

toJsTree :: Maybe String -> TreeNode -> TreeNode
toJsTree maybeSurname (TreeNode x) =
  unsafeCoerce { name : name
               , value : foldl (+) 0 $ [x.value] <> map treeValue x.children
               , children : (map (toJsTree (Just name)) x.children)
               }
    where
      name = maybe "" (\x -> x <> ">") maybeSurname  <> x.name

data TreeNode = TreeNode { name     :: String
                 , value    :: Int
                 , children :: Array TreeNode
                 }


instance decodeTreeNode :: DecodeJson TreeNode where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .: "label"
    value <- obj .: "value"
    children <- obj .: "children"
    pure $ TreeNode {name, value, children}



treeNode :: String -> Int -> Array TreeNode -> TreeNode
treeNode n v ts = TreeNode {name : n, value:v, children:ts}

treeLeaf :: String -> Int -> TreeNode
treeLeaf n v = TreeNode { name : n, value : v, children : []}


-- | TODO
-- https://ecomfe.github.io/echarts-examples/public/data/asset/data/life-expectancy-table.json
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=scatter3D-dataset&gl=1

