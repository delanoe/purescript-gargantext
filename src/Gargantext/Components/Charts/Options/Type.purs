module Gargantext.Components.Charts.Options.Type where

import Prelude

import Gargantext.Components.Charts.Options.Color (Color)
import Gargantext.Components.Charts.Options.Data (DataLegend)
import Gargantext.Components.Charts.Options.Font (TextStyle, Tooltip, ToolBox)
import Gargantext.Components.Charts.Options.Legend (LegendType, Orient, SelectedMode)
import Gargantext.Components.Charts.Options.Position (LeftRelativePosition, Position, TopRelativePosition)
import Gargantext.Components.Charts.Options.Series (Series)
import Gargantext.Types (class Optional)
import React as R
import Unsafe.Coerce (unsafeCoerce)

newtype ChartAlign = ChartAlign String

-- TODO: Maybe is not working here => use Optional

type Echarts =
  { option      :: Option       --  PropTypes.object.isRequired,
--, className   :: Maybe String
--, style       :: Maybe String  -- objealect-black-altdarkmincnaquadahherry-blossomect,
--, theme       :: Maybe String
--, group       :: Maybe String
--, initOpts    :: Maybe String -- PropTypes.object,
--, notMerge    :: Maybe Boolean
--, lazyUpdate  :: Maybe Boolean
--, loading     :: Maybe Boolean
--, optsLoading :: Maybe OptsLoading --  PropTypes.object,
--, onReady     :: Maybe String   --  PropTypes.func,
--, resizable   :: Maybe Boolean  -- PropTypes.bool,
--, onEvents    :: Maybe String   -- PropTypes.object
  }

type Option =
  { title    :: Title
  , legend   :: Legend
  , tooltip  :: Tooltip
  , grid     :: Grid
  , xAxis    :: XAxis
  , yAxis    :: YAxis
  , series   :: Array Series
  , dataZoom :: Array DataZoom
  , children :: R.Children
  , toolbox   :: ToolBox
  }

type Title =
  { id :: String -- None by default
  , show :: Boolean -- default True
  , text :: String -- default ''
  , link :: String -- default ''
  , target :: String -- default 'blank'
  , textStyle :: TextStyle
  , subtext :: String -- default ''
  , sublink :: String -- default ''
  , subtarget :: String -- default 'blank'
  , subtextStyle :: TextStyle
  , padding :: Number -- default '5'
  , itemGap :: Number -- default '10'
  , zlevel :: Number -- default '0'
  , z :: Number -- default '2'
  , left :: Position LeftRelativePosition -- default 'auto'
  , top :: Position TopRelativePosition -- default 'auto'
  , right :: Position Unit -- default 'auto'
  , bottom :: Position Unit -- default 'auto'
  , backgroundColor :: Color -- default 'transparent''
  , borderColor :: Color -- default '#ccc'
  , borderWidth :: Number -- default '1'
  , borderRadius :: Number -- default 0; data NumberOrArray = Number | Array Number
  , shadowBlur :: Number
  , shadowColor :: Color
  , shadowOffsetX :: Number
  , shadowOffsetY :: Number
  }

type OptsLoading =
  { text      :: String
  ,  color     :: Color  --- color
  ,  textColor :: Color --color
  ,  maskColor :: Color --color
  ,  zlevel    :: Int
  }

type DataZoom =
  {"type"      :: String
  , xAxisIndex :: Int
  , filterMode :: String
  , start      :: Int
  , end        :: Int
  }

type Grid =
  {containLabel :: Boolean
  }

type Legend =
  {
    id :: String
  , "type" :: LegendType
  , show :: Boolean
  , zlevel :: Number
  , z :: Number
  , left :: Position LeftRelativePosition -- default 'auto
  , top :: Position TopRelativePosition
  , right :: Position Unit
  , bottom :: Position Unit
  , width :: Position Unit
  , height :: Position Unit
  , orient :: Orient
  , align :: Position LeftRelativePosition
  , padding :: Number
  , itemGap :: Number
  , itemWidth :: Number
  , itemHeight :: Number
--, formatter :: Maybe String
  , selectedMode :: SelectedMode
  , inactiveColor :: Color
--, selected :: Maybe String -- object
  , textStyle :: TextStyle
  , "data" :: Array DataLegend
  }

type AxisTick =
  { alignWithLabel :: Boolean
  }

data XAxis

type XAxisOptional =
  ( "data"    :: Array String -- DataAxis
  , "type"    :: String
  , axisTick  :: AxisTick
  , name      :: String
  , min       :: Int
  , position  :: String
  , axisLabel :: AxisLabel
  , show      :: Boolean
  )

xAxis :: forall o. Optional o XAxisOptional => Record o -> XAxis
xAxis = unsafeCoerce

data YAxis

type YAxisOptional =
  ( "type"    :: String
  , name      :: String
  , min       :: Int
  , position  :: String
  , axisLabel :: AxisLabel
  , show      :: Boolean
  )

yAxis :: forall o. Optional o YAxisOptional => Record o -> YAxis
yAxis = unsafeCoerce

type AxisLabel =
  { formatter :: String -- string or function
  }

type Rich = {}
