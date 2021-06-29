module Gargantext.Components.Charts.Options.Type where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import Gargantext.Components.Charts.Options.Color (Color)
import Gargantext.Components.Charts.Options.Data (DataLegend)
import Gargantext.Components.Charts.Options.Font (TextStyle, Tooltip, ToolBox)
import Gargantext.Components.Charts.Options.Legend (LegendType, Orient, SelectedMode)
import Gargantext.Components.Charts.Options.Position (LeftRelativePosition, Position, TopRelativePosition)
import Gargantext.Components.Charts.Options.Series (Series)
import Gargantext.Types (class Optional)
import React as R
import Unsafe.Coerce (unsafeCoerce)

-- | https://echarts.apache.org/en/api.html#echartsInstance
foreign import data EChartsInstance :: Type

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
  , onEvents    :: OnEvents   -- PropTypes.object
  , ref         :: Effect Unit
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

---

-- | @XXX "echarts-for-react" third party library does not have an event
-- |      dictionary
-- |      these values had been picked from what we gather in the dist file
-- |      "echarts/dist/echarts.common.js" and
-- |      https://echarts.apache.org/en/api.html#events
type OnEvents =
  { click     :: Effect Unit
  -- ...
  }

-- | @XXX "echarts-for-react" third party library bases on "apache-echarts"
-- |      does not have strongly typed signature, nor determined arity
-- |      (actual runtime event contains more key than what their docs describe)
-- |
-- | https://echarts.apache.org/en/api.html#events.Mouse%20events
type MouseEvent =
  { borderColor       :: Nullable String
  , color             :: String
  , componentIndex    :: Int
  , componentSubType  :: String
  , componentTyp      :: String
  -- , data           ::  -- Object
  , dataIndex         :: Int
  , dataType          :: Nullable String
  -- , dimensionNames :: -- Array
  -- , encore         :: -- Object
  -- , event          :: -- instanceof Event
  -- , marker         :: -- String
  , name              :: String
  , seriesId          :: Nullable String
  , seriesIndex       :: Int
  , seriesName        :: String
  , seriesType        :: String
  , type              :: String
  , value             :: String -- or Array ??
  }

----

-- | @XXX partial definition given by the third library author
-- |      POJO containing a mix of ReactElement field and custom method attached
-- |
-- | https://github.com/hustcc/echarts-for-react#component-api--echarts-api
type EChartRef =
  ( getEchartsInstance :: Effect EChartsInstance
  -- ...
  )
