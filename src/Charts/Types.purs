module Charts.Types where

import Unsafe.Coerce

import CSS (Color, FontStyle(..), FontWeight(..), Prefixed(..), Value(..), toHexString)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Prelude ((<>), class Show, show, ($), Unit, (<<<))

type NumberOrArray = Either Number (Array Number)

data TopRelativePosition = Top | Middle | Bottom
instance showTopRelativePosition :: Show TopRelativePosition
  where show (Top) = "top"
        show (Middle) = "middle"
        show (Bottom) = "bottom"

data LeftRelativePosition = LeftPos | Center | RightPos
instance showLeftRelativePosition :: Show LeftRelativePosition
  where show (LeftPos) = "left"
        show (Center) = "center"
        show (RightPos) = "right"

data Align p = Auto | Relative p

newtype ChartAlign = ChartAlign String

newtype ChartColor = ChartColor String

renderChartColor :: Color -> ChartColor
renderChartColor = ChartColor <<< toHexString

newtype ChartFontStyle = ChartFontStyle String

renderChartFontStyle :: FontStyle -> ChartFontStyle
renderChartFontStyle (FontStyle (Value (Plain "italic"))) = ChartFontStyle "italic"
renderChartFontStyle (FontStyle (Value (Plain "oblique"))) = ChartFontStyle "oblique"
renderChartFontStyle _ = ChartFontStyle "normal"

newtype ChartFontWeight = ChartFontWeight String

renderChartFontWeight :: FontWeight -> ChartFontWeight
renderChartFontWeight (FontWeight (Value (Plain "bold"))) = ChartFontWeight "bold"
renderChartFontWeight (FontWeight (Value (Plain "bolder"))) = ChartFontWeight "bolder"
renderChartFontWeight (FontWeight (Value (Plain "lighter"))) = ChartFontWeight "lighter"
renderChartFontWeight  _ = ChartFontWeight "normal"

foreign import data Position :: Type -> Type

renderNumber :: forall r. Number -> Position r
renderNumber = unsafeCoerce

renderPercentage :: forall r. Number -> Position r
renderPercentage n = unsafeCoerce $ (show n) <> "%"

renderRelativePosition :: forall a. Show a => Align a -> Position a
renderRelativePosition (Auto) = unsafeCoerce "auto"
renderRelativePosition (Relative r) = unsafeCoerce $ show r

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
  , borderRadius :: NumberOrArray -- default 0; data NumberOrArray = Number | Array Number
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
