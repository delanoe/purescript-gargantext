module Charts.Types where

import Data.Maybe(Maybe(..))
import CSS (Color)

-- eCharts Props

-- className :: String -> from React DOM Props
--    style     :: String,  -- object,

    -- group     :: String,
    -- option    :: Option, --  PropTypes.object.isRequired,
    -- initOpts  :: String, -- PropTypes.object,
    -- notMerge  :: Boolean,
    -- lazyUpdate:: Boolean
    -- loading   :: Boolean,
    -- optsLoading::  OptsLoading, --  PropTypes.object,
    -- onReady    :: String, --  PropTypes.func,
    -- resizable  :: Boolean, -- PropTypes.bool,
    -- onEvents   :: String --  PropTypes.object

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

type OptsLoading =
  { text      :: String,
    color     :: Color,  --- color
    textColor :: Color, --color
    maskColor :: Color, --color
    zlevel    :: Int
  }

type Option =
  { title    :: Maybe Title
  , legend   :: Maybe Legend
  , tooltip  :: Tooltip
  , grid     :: Grid
  , xAxis    :: XAxis
  , yAxis    :: YAxis
  , series   :: Array Series
  , dataZoom :: Array DataZoom
  }

type Title =
  { text         :: String
  , show         :: Boolean
  , link         :: String
  , target       :: String
  , textStyle    :: TextStyle
  , subtext      :: String
  , sublink      :: String
  , subtarget    :: String
  , subtextStyle :: SubtextStyle
  , padding      :: Number
  , itemGap      :: Number
  , zlevel       :: Number
  , z            :: Number
  , left         :: Number
  , top          :: Number
  , right        :: Number
  , bottom       :: Number
  , backgroundColor :: Color
  , borderColor     :: Color
  , borderWidth     :: Number
  , borderRadius    :: Number -- data NumberOrArray = Number | Array Number
  , shadowBlur      :: Number
  , shadowColor     :: Color
  , shadowOffsetX   :: Number
  , shadowOffsetY   :: Number
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
  {"type"  :: String
  , show   :: Boolean
  , zlevel :: Maybe Number
  , z      :: Maybe Number
  , left   :: Maybe Number
  , top    :: Maybe Number
  , right  :: Maybe Number
  , bottom :: Maybe Number
  , width  :: Maybe Number
  , height :: Maybe Number
  , orient :: Maybe String
  , align  :: Maybe String
  , padding       :: Maybe Number
  , itemGap       :: Maybe Number
  , itemWidth     :: Maybe Number
  , itemHeight    :: Maybe Number
  , formatter     :: Maybe String
  , selectedMode  :: Maybe Boolean
  , inactiveColor :: Maybe Color
  , selected      :: Maybe String -- object
  , "data"        :: Maybe (Array Data)
  }

type Data =
  { name      :: String
  , icon      :: Maybe String
  , textStyle :: Maybe {}
  }

type SubtextStyle =
  { color      :: Color
  , fontStyle  :: String
  , fontWeight :: String
  , fontFamily :: String
  , fontSize   :: Int
  , align      :: String
  , verticalAlign :: String
  , lineHeight    :: Number
  , width         :: Number
  , height        :: Number
  , textBorderColor :: String
  , textBorderWidth :: Number
  , textShadowColor :: String
  , textShadowBlur  :: Number
  , textShadowOffsetX :: Number
  , textShadowOffsetY :: Number
  , rich              :: Rich
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

-- Props

type TextStyle =
  { color :: Color
  , fontStyle :: String
  , fontWeight :: String
  , fontFamily :: String
  , fontSize :: Int
  , align :: String
  , verticalAlign :: String
  , lineHeight :: Int
  , width :: Int
  , height         :: Int
  , textBorderColor :: String
  , textBorderWidth :: Int
  , textShadowColor :: String
  , textShadowBlur  :: Int
  , textShadowOffsetX :: Int
  , textShadowOffsetY :: Int
  , rich              :: Rich
  }

type Rich = {}
