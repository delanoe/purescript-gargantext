module Gargantext.Components.Charts.Options.Type where

import Prelude

import CSS (Color)
import Data.Maybe (Maybe)
import Gargantext.Components.Charts.Options.Color (ChartColor)
import Gargantext.Components.Charts.Options.Data (DataN, DataV)
import Gargantext.Components.Charts.Options.Font (TextStyle)
import Gargantext.Components.Charts.Options.Legend (LegendType, Orient, SelectedMode)
import Gargantext.Components.Charts.Options.Position (LeftRelativePosition, Position, TopRelativePosition)
import Gargantext.Components.Charts.Options.Series (Series)
import React as R
newtype ChartAlign = ChartAlign String

type Echarts =
  { className   :: Maybe String
  ,  style       :: Maybe String  -- objealect-black-altdarkmincnaquadahherry-blossomect,
  ,  theme       :: Maybe String
  , group       :: Maybe String
  , option      :: Option       --  PropTypes.object.isRequired,
  , initOpts    :: Maybe String -- PropTypes.object,
  , notMerge    :: Maybe Boolean
  , lazyUpdate  :: Maybe Boolean
  , loading     :: Maybe Boolean
  , optsLoading :: Maybe OptsLoading --  PropTypes.object,
  , onReady     :: Maybe String   --  PropTypes.func,
  , resizable   :: Maybe Boolean  -- PropTypes.bool,
  , onEvents    :: Maybe String   -- PropTypes.object
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
  , backgroundColor :: ChartColor -- default 'transparent''
  , borderColor :: ChartColor -- default '#ccc'
  , borderWidth :: Number -- default '1'
  , borderRadius :: Number -- default 0; data NumberOrArray = Number | Array Number
  , shadowBlur :: Number
  , shadowColor :: ChartColor
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
  , formatter :: Maybe String
  , selectedMode :: SelectedMode
  , inactiveColor :: ChartColor
  , selected :: Maybe String -- object
  , textStyle :: TextStyle
  , "data" :: Array DataN
  }

type Tooltip =
  { trigger   :: String
  , formatter :: Maybe String -- TODO function
  }

type XAxis =
  { "data"   :: Array DataV
  , "type"   :: String
  , axisTick :: AxisTick
  , show :: Boolean
  }

type AxisTick =
  {
    alignWithLabel :: Boolean
  }

type YAxis =
  { "type"    :: String
  , name      :: String
  , min       :: Int
  , position  :: String
  , axisLabel :: AxisLabel
  , show :: Boolean
  }

type AxisLabel =
  { formatter :: String -- string or function
  }

type Rich = {}
