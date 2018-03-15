module Chart where

import Prelude

import CSS (Color, white)
import React (ReactClass, ReactElement, createElementDynamic)
import React.DOM.Props (Props, unsafeFromPropsArray, unsafeMkProps)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import React as R
import React.DOM (p)
import React.DOM.Props (Props, className, unsafeFromPropsArray)
import ReactDOM as RDOM
import Thermite (Render, Spec, createReactSpec, defaultPerformAction, simpleSpec)

-- eCharts Props

-- className :: String -> from React DOM Props
--    style     :: String,  -- object,

theme :: String -> Props
theme = unsafeMkProps "theme"

group :: String -> Props
group = unsafeMkProps "group"

    -- group     :: String,
    -- option    :: Option, --  PropTypes.object.isRequired,
    -- initOpts  :: String, -- PropTypes.object,
    -- notMerge  :: Boolean,
    -- lazyUpdate:: Boolean,
    -- loading   :: Boolean,
    -- optsLoading::  OptsLoading, --  PropTypes.object,
    -- onReady    :: String, --  PropTypes.func,
    -- resizable  :: Boolean, -- PropTypes.bool,
    -- onEvents   :: String --  PropTypes.object

type EchartsProps eff =
  { className :: String,
    style     :: String,  -- object,
    theme     :: String,
    group     :: String,
    option    :: Option, --  PropTypes.object.isRequired,
    initOpts  :: String, -- PropTypes.object,
    notMerge  :: Boolean,
    lazyUpdate:: Boolean,
    loading   :: Boolean,
    optsLoading::  OptsLoading, --  PropTypes.object,
    onReady    :: String, --  PropTypes.func,
    resizable  :: Boolean, -- PropTypes.bool,
    onEvents   :: String --  PropTypes.object
  }

type OptsLoading =
  { text :: String,
    color :: Color,  --- color
    textColor :: Color, --color
    maskColor:: Color, --color
    zlevel :: Int
  }

type Option =
  { title :: Title
  , legend :: Legend
  , tooltip :: Tooltip
  , grid :: Grid
  , xAxis :: Array XAxis
  , yAxis :: Array YAxis
  , series :: Array Series
  , dataZoom :: Array DataZoom
  }


type DataZoom =
  {"type":: String
  , xAxisIndex:: Int
  , filterMode:: String
  , start:: Int
  , end:: Int
  }

type Grid =
  {containLabel :: Boolean
  }

type Legend =
  {"type" :: String
  , show :: Boolean
  , zlevel :: Number
  , z :: Number
  , left :: Number
  , top :: Number
  , right :: Number
  , bottom :: Number
  , width :: Number
  , height :: Number
  , orient :: String
  , align :: String
  , padding :: Number
  , itemGap :: Number
  , itemWidth :: Number
  , itemHeight :: Number
  , formatter :: String
  , selectedMode :: Boolean
  , inactiveColor :: Color
  , selected :: String -- object
  , "data" :: Array Data
  }

type Data =
  {name :: String
  , icon :: String
  , textStyle :: {}
  }



type SubtextStyle =
  { color :: Color
  , fontStyle :: String
  , fontWeight :: String
  , fontFamily :: String
  , fontSize :: Int
  , align :: String
  , verticalAlign :: String
  , lineHeight :: Number
  , width :: Number
  , height :: Number
  , textBorderColor :: String
  , textBorderWidth :: Number
  , textShadowColor :: String
  , textShadowBlur :: Number
  , textShadowOffsetX :: Number
  , textShadowOffsetY :: Number
  , rich :: Rich
  }


type Tooltip =
  { trigger :: String
  , formatter :: String -- TODO function
  }

type XAxis =
  { "data" :: Array String
  , "type" :: String
  , axisTick :: AxisTick
  }
type AxisTick =
  {
    alignWithLabel :: Boolean
  }

type YAxis =
  { "type" :: String
  , name :: String
  , min :: Int
  , position :: String
  , axisLabel :: AxisLabel
  }

type AxisLabel =
  { formatter :: String -- string or function
  }


type Series =
  {name :: String
  , "type" :: String
  , "data" :: Array Int
  }

type Title =
  { text :: String
  , show :: Boolean
  , link :: String
  , target :: String
  , textStyle :: TextStyle
  , subtext :: String
  , sublink :: String
  , subtarget :: String
  , subtextStyle :: SubtextStyle
  , padding :: Number
  , itemGap :: Number
  , zlevel :: Number
  , z :: Number
  , left :: Number
  , top :: Number
  , right :: Number
  , bottom :: Number
  , backgroundColor :: Color
  , borderColor :: Color
  , borderWidth :: Number
  , borderRadius :: Number -- NumberOrArray
  , shadowBlur :: Number
  , shadowColor :: Color
  , shadowOffsetX :: Number
  , shadowOffsetY :: Number
  }

-- data NumberOrArray = Number | Array Number



type Rich = {}


foreign import eChartsClass :: forall props. ReactClass props

echarts :: forall eff. Array Props -> ReactElement
echarts p = createElementDynamic eChartsClass (unsafeFromPropsArray p) []

-- Props

loading :: Boolean -> Props
loading = unsafeMkProps "loading"

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
  , height :: Int
  , textBorderColor :: String
  , textBorderWidth :: Int
  , textShadowColor :: String
  , textShadowBlur :: Int
  , textShadowOffsetX :: Int
  , textShadowOffsetY :: Int
  , rich :: Rich
  }

foreign import data TextStyleProps :: Type

textStyle :: Array Props -> Props
textStyle = unsafeMkProps "textStyle"

subTextStyle :: Array Props -> Props
subTextStyle = unsafeMkProps "subTextStyle"

color :: Color -> Props
color = unsafeMkProps "color"

align :: String -> Props
align = unsafeMkProps "align"

option :: Array Props -> Props
option = unsafeMkProps "option" <<< unsafeFromPropsArray

ts :: Props
ts = textStyle [color white, align "left"]

title :: Array Props -> Props
title = unsafeMkProps "title" <<< unsafeFromPropsArray

text :: String -> Props
text = unsafeMkProps "text"


tooltip :: Array Props -> Props
tooltip = unsafeMkProps "tooltip" <<< unsafeFromPropsArray

trigger :: String -> Props
trigger = unsafeMkProps "trigger"

grid :: Array Props -> Props
grid = unsafeMkProps "grid" <<< unsafeFromPropsArray

containLabel :: Boolean -> Props
containLabel = unsafeMkProps "containLabel"

legend :: Array Props -> Props
legend = unsafeMkProps "legend" <<< unsafeFromPropsArray

data' :: forall a. a -> Props
data' = unsafeMkProps "data"

xAxis :: Array Props -> Props
xAxis ap = unsafeMkProps "xAxis" [unsafeFromPropsArray ap]

type' :: String -> Props
type' = unsafeMkProps "type"

axisTick :: Array Props -> Props
axisTick = unsafeMkProps "axisTick" <<< unsafeFromPropsArray

alignWithLabel :: Boolean -> Props
alignWithLabel = unsafeMkProps "alignWithLabel"

xAxisIndex :: Int -> Props
xAxisIndex = unsafeMkProps "xAxisIndex"

filterMode :: String -> Props
filterMode = unsafeMkProps "filterMode"

start :: Int -> Props
start = unsafeMkProps "start"

end :: Int -> Props
end = unsafeMkProps "end"

dataZoom :: Array Props -> Props
dataZoom = unsafeMkProps "dataZoom"

name :: String -> Props
name = unsafeMkProps "name"

position :: String -> Props
position = unsafeMkProps "position"

axisLabel :: Array Props -> Props
axisLabel = unsafeMkProps "axisLabel" <<< unsafeFromPropsArray

formatter :: String -> Props
formatter = unsafeMkProps "formatter"

min :: Int -> Props
min = unsafeMkProps "min"

yAxis :: Array Props -> Props
yAxis = unsafeMkProps "yAxis"

series :: Array Props -> Props
series = unsafeMkProps "series"

label :: Array Props -> Props
label = unsafeMkProps "label"

normal :: Array Props -> Props
normal = unsafeMkProps "normal"

showp :: Boolean -> Props
showp = unsafeMkProps "show"

lineStyle :: Array Props -> Props
lineStyle = unsafeMkProps "lineStyle" <<< unsafeFromPropsArray

width :: Int -> Props
width = unsafeMkProps "width"

shadowColor :: String -> Props
shadowColor = unsafeMkProps "shadowColor"

shadowBlur :: Int -> Props
shadowBlur = unsafeMkProps "shadowBlur"

shadowOffsetY :: Int -> Props
shadowOffsetY = unsafeMkProps "shadowOffsetY"

yAxisIndex :: Int -> Props
yAxisIndex = unsafeMkProps "yAxisIndex"



-------------------


      -- [ p''
      -- , ex1
      -- , p''
      -- ]

histogram :: ReactElement
histogram = echarts
     [ option
       [ tooltip [trigger "axis"]
       , grid [containLabel true]
       , legend [data' ["Map terms coverage", "Favorites", "All"]]
       -- , legend [data' ["Map Terms coverage", "Favorites", "All"]]
       , xAxis
         [ type' "category"
         , axisTick [alignWithLabel true]
         , data' ["Jan" , "Feb", "Mar" , "Apr"
                 , "May", "Jun", "July", "Aug"
                 , "Sep", "Oct", "Nov" , "Dec"
                 ]
         ]
       , dataZoom [dz1,dz1,dz2,dz2]
       , yAxis [ya1, ya2]
       , series [sd1, sd2, sd3]
       ]
     ]

dz1 = unsafeFromPropsArray
      [ type' "slider"
      , xAxisIndex 0
      , filterMode "empty"
      , start 0
      , end 100
      ]

dz2 = unsafeFromPropsArray
      [ type' "inside"
      , xAxisIndex 0
      , filterMode "empty"
      , start 0
      , end 100
      ]

ya1 = unsafeFromPropsArray
      [ type' "value"
      , name "Score metric"
      , min 0
      , position "right"
      , axisLabel [formatter "{value}"]
      ]

ya2 = unsafeFromPropsArray
      [ type' "value"
      , name "Publications (by year)"
      , min 0
      , position "left"
      , axisLabel [formatter "{value}"]
      ]

sd1 = unsafeFromPropsArray
      [ name "Map terms coverage"
      , type' "line"
      , label [normal[showp true, position "top"]]
      , lineStyle [ normal
                    [ width 3
                    , shadowColor "rgba(0,0,0,0.4)"
                    , shadowBlur 10
                    , shadowOffsetY 10
                    ]]
      , data' [95, 80, 75, 35, 30, 50, 70, 80, 95, 95, 95, 99]
      ]


sd3 = unsafeFromPropsArray
      [ name "All"
      , type' "bar"
      , label [normal[showp true, position "top"]]
      , yAxisIndex 1
      , data' [201, 222, 223, 777, 244, 255, 555, 879, 938, 1364, 1806, 2324]
      ]


sd2 = unsafeFromPropsArray
      [ name "Favorites"
      , type' "bar"
      , label [normal[showp true, position "top"]]
      , yAxisIndex 1
      , data' [22, 22, 23, 77, 24, 55, 139, 350, 150, 164, 106, 224]
      ]


p'' :: ReactElement
p'' = p [] []
