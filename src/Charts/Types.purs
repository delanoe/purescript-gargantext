module Charts.Types where

import Prelude ((<>), show, ($))
import Data.Maybe(Maybe)
import Data.Either(Either)
import Data.String (toLower)
import Data.Generic (class Generic, gShow)
import CSS (Color)

type NumberOrArray = Either Number (Array Number)

data Top = Top | Middle | Bottom
derive instance genericTop :: Generic Top

data Position relative = Flat Number | Percent Number | Relative relative -- 20 | 20% | top

class InitialB r
instance nightOfNumber :: InitialB Number
instance runningInTheString :: InitialB String

displayTopPosition :: forall a. InitialB a => Position Top -> a
displayTopPosition (Flat n) = n
displayTopPosition (Percent n) = (show n) <> "%"
displayTopPosition (Relative r) = toLower $ gShow r

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
  {
    id :: Maybe String
  , show :: Boolean -- default True
  , text :: Maybe String -- default ''
  , link :: Maybe String -- default ''
  , target :: String -- default 'blank'
  , textStyle :: TextStyle
  , subtext :: String -- default ''
  , sublink :: String -- default ''
  , subtarget :: String -- default 'blank'
  , subtextStyle :: SubtextStyle
  , padding :: Number -- default '5'
  , itemGap :: Number -- default '10'
  , zlevel :: Number -- default '0'
  , z :: Number -- default '2'
  , left :: Number -- default 'auto'
  , top :: Number -- default 'auto'
  , right :: Number -- default 'auto'
  , bottom :: Number -- default 'auto'
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
