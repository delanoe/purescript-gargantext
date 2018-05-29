module Charts.Type where

import Charts.Font

import CSS (Color)
import Charts.Color (ChartColor)
import Charts.Legend (LegendType)
import Charts.Position (LeftRelativePosition, Position, TopRelativePosition)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Prelude (Unit, (<<<))


newtype ChartAlign = ChartAlign String

type Echarts =
  { className   :: Maybe String,
    style       :: Maybe String,  -- objealect-black-altdarkmincnaquadahherry-blossomect,
    theme       :: Maybe String,
    group       :: Maybe String,
    option      :: Option, --  PropTypes.object.isRequired,
    initOpts    :: Maybe String, -- PropTypes.object,
    notMerge    :: Maybe Boolean,
    lazyUpdate  :: Maybe Boolean,
    loading     :: Maybe Boolean,
    optsLoading :: Maybe OptsLoading, --  PropTypes.object,
    onReady     :: Maybe String, --  PropTypes.func,
    resizable   :: Maybe Boolean, -- PropTypes.bool,
    onEvents    :: Maybe String --  PropTypes.object
  }

type Option =
  { title    :: Title
  , legend   :: Maybe Legend
  , tooltip  :: Tooltip
  , grid     :: Grid
  , xAxis    :: XAxis
  , yAxis    :: YAxis
  , series   :: Array Series
  , dataZoom :: Array DataZoom
  }

type Title =
  {
    id :: String -- None by default
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
  { text      :: String,
    color     :: Color,  --- color
    textColor :: Color, --color
    maskColor :: Color, --color
    zlevel    :: Int
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
  , orient :: Maybe String
  , align :: Maybe String
  , padding :: Maybe Number
  , itemGap :: Maybe Number
  , itemWidth :: Maybe Number
  , itemHeight :: Maybe Number
  , formatter :: Maybe String
  , selectedMode :: Maybe Boolean
  , inactiveColor :: Maybe Color
  , selected :: Maybe String -- object
  , "data" :: Maybe (Array Data)
  }

type Data =
  { name      :: String
  , icon      :: Maybe String
  , textStyle :: Maybe {}
  }

type TextStyle =
  { color      :: ChartColor
  , fontStyle  :: ChartFontStyle
  , fontWeight :: ChartFontWeight
  , fontFamily :: String
  , fontSize   :: Int
  , align      :: Position LeftRelativePosition
  , verticalAlign :: Position TopRelativePosition
  , lineHeight    :: Position Unit
  , width         :: Position Unit
  , height        :: Position Unit
  , textBorderColor :: ChartColor
  , textBorderWidth :: Number
  , textShadowColor :: ChartColor
  , textShadowBlur  :: ChartColor
  , textShadowOffsetX :: Number
  , textShadowOffsetY :: Number
  }

type Tooltip =
  { trigger   :: String
  , formatter :: Maybe String -- TODO function
  }

type XAxis =
  { "data"   :: Array Data
  , "type"   :: String
  , axisTick :: AxisTick
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
  }

type AxisLabel =
  { formatter :: String -- string or function
  }

type Series =
  { name   :: String
  , "type" :: String
  , "data" :: Array Int
  }

type Rich = {}
