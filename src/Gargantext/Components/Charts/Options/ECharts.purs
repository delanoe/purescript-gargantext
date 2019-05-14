module Gargantext.Components.Charts.Options.ECharts where

import Prelude

import CSS (italic)
import CSS.Common (normal)
import Data.Array (length)
import Gargantext.Components.Charts.Options.Color (transparent, violet, black)
import Gargantext.Components.Charts.Options.Data (DataLegend, DataAxis, dataSerie)
import Gargantext.Components.Charts.Options.Font (IconOptions(..), Shape(..), TextStyle, chartFontStyle, chartFontWeight, icon, mkTooltip, Tooltip, ToolBox, mkToolBox)
import Gargantext.Components.Charts.Options.Legend (legendType, LegendMode(..), PlainOrScroll(..), selectedMode, Orientation(..), orient)
import Gargantext.Components.Charts.Options.Position (Align(..), LeftRelativePosition(..), TopRelativePosition(..), numberPosition, percentPosition, relativePosition)
import Gargantext.Components.Charts.Options.Series (Series, SeriesName, Chart(..), seriesPieD1)
import Gargantext.Components.Charts.Options.Type (DataZoom, Echarts, Legend, Option, Title, XAxis, YAxis, xAxis, yAxis)
import React (unsafeCreateElementDynamic)
import React as R
import Unsafe.Coerce (unsafeCoerce)


foreign import eChartsClass :: R.ReactClass Echarts

chart :: Options -> R.ReactElement
chart = echarts <<< chartWith <<< opts


chartWith :: Option -> Echarts
chartWith option =
  { option
--, className : Nothing
--, style     : Nothing
--, theme     : Nothing
--, group     : Nothing
--, initOpts  : Nothing
--, notMerge  : Nothing
--, lazyUpdate: Nothing
--, loading   : Nothing
--, optsLoading: Nothing
--, onReady    : Nothing
--, resizable  : Nothing
--, onEvents   : Nothing
  }

echarts :: Echarts -> R.ReactElement
echarts c = unsafeCreateElementDynamic (unsafeCoerce eChartsClass) c []

type MainTitle = String
type SubTitle  = String

title :: MainTitle -> SubTitle -> Title
title mainTitle subTitle =
  {
    id: ""
   ,show: true
   ,text: mainTitle
   ,link: ""
   ,target: "blank"
   ,textStyle: textStyle
   ,subtext: subTitle
   ,sublink: ""
   ,subtarget: "blank"
   ,subtextStyle: textStyle2
   ,padding: 10.0
   ,itemGap: 0.0
   ,zlevel: 2.0
   ,z: 2.0
   ,left: relativePosition (Relative RightPos)
   ,top: relativePosition (Relative Top)
   ,right: numberPosition 60.0
   ,bottom: percentPosition 40.0
   ,backgroundColor: transparent
   ,borderColor: transparent
   ,borderWidth: 0.0
   ,borderRadius: 0.0
   ,shadowBlur: 0.0
   ,shadowColor: transparent
   ,shadowOffsetX: 0.0
   ,shadowOffsetY: 0.0
  }

legend :: Legend
legend =
  {
    id: "Muda"
   ,"type": legendType Plain
   , show: true
   , zlevel: 0.0
   , z: 2.0
   , left: relativePosition Auto
   , top: relativePosition Auto
   , right: relativePosition Auto
   , bottom: relativePosition Auto
   , width: relativePosition Auto
   , height: relativePosition Auto
   , orient: orient Horizontal
   , align: relativePosition Auto
   , padding: 5.0
   , itemGap: 10.0
   , itemWidth: 25.0
   , itemHeight: 14.0
 --, formatter: Nothing
   , selectedMode: selectedMode $ Bool true
   , inactiveColor: violet
 --- selected: Nothing
   , textStyle: textStyle
   , "data": [data1]
  }

data1 :: DataLegend
data1 = {name: "Map terms coverage", icon: icon $ Shape Circle, textStyle: textStyle'}

data2 :: DataLegend
data2 = {name: "Favorites", icon: icon $ Shape Circle, textStyle: textStyle'}

data3 :: DataLegend
data3 = {name: "Test", icon: icon $ Shape Diamond, textStyle: textStyle'}


yAxisVoid :: YAxis
yAxisVoid = yAxis
  { "type": ""
  , name: ""
  , min: 0
  , position: ""
  , axisLabel: {formatter: ""}
  , show: false
  }

xAxis' :: Array String -> XAxis
xAxis' [] = unsafeCoerce {show:false}
xAxis' xs = xAxis
            { "data": xData xs
            , "type": "category"
            , axisTick: {alignWithLabel: true}
            , show: true
            }
  where
    xData :: Array String -> Array DataAxis
    xData = map (\x -> {value : x, textStyle : textStyle})

-- TODO try to use Optional
yAxis' :: { position :: String
          , show     :: Boolean
          , min      :: Int
          } -> YAxis
yAxis' {position, show, min} = yAxis
  { "type": "value"
  , name: "data"
  , min: min
  , axisLabel: {formatter: "{value}"}
  , position
  , show
  }

data Options = Options
  { mainTitle :: MainTitle
  , subTitle  :: SubTitle
  , xAxis     :: XAxis
  , yAxis     :: YAxis
  , series    :: Array Series
  , addZoom   :: Boolean
  , tooltip   :: Tooltip
  }

tooltipTriggerAxis :: Tooltip
tooltipTriggerAxis = mkTooltip { trigger: "axis"}

opts :: Options -> Option
opts (Options { mainTitle
              , subTitle
              , xAxis
              , yAxis
              , series
              , tooltip
              , addZoom
              }) =
  { title: title mainTitle subTitle
  , legend
  , tooltip
  , grid: {containLabel: true}
  , series
  , xAxis
  , yAxis
  , dataZoom: if addZoom then [zoom Slider, zoom Inside] else []
  , children : unsafeCoerce [] -- TODO
  , toolbox: mkToolBox
  }

data Zoom = Slider | Inside

instance showZoom :: Show Zoom where
  show Slider = "slider"
  show Inside = "inside"

zoom :: Zoom -> DataZoom
zoom z = {
  "type": show z
  ,xAxisIndex: 0
  ,filterMode: "empty"
  ,start: 0
  ,end: 100
  }


seriesPie :: Series
seriesPie = seriesPieD1
  { name: "Pie name" }
  (dataSerie <$> [ {name: "t1", value: 50.0}
                 , {name: "t2", value: 45.0}
                 , {name: "t3", value: 65.0}
                 , {name: "t4", value: 15.0}
                 , {name: "t5", value: 23.0}
                 ])


textStyle2 :: TextStyle
textStyle2 =
  { color: black
  , fontStyle: chartFontStyle italic
  , fontWeight: chartFontWeight normal
  , fontFamily: "sans-serif"
  , fontSize: 11
  , align: relativePosition $ Relative RightPos
  , verticalAlign: relativePosition $ Relative Bottom
  , lineHeight: percentPosition 0.0
  , width: percentPosition 100.0
  , height: percentPosition 100.0
  , textBorderColor: black
  , textBorderWidth: 0.0
  , textShadowColor: black
  , textShadowBlur: black
  , textShadowOffsetX: 0.0
  , textShadowOffsetY: 0.0
  }

textStyle' :: TextStyle
textStyle' =
  { color: black
  , fontStyle: chartFontStyle normal
  , fontWeight: chartFontWeight normal
  , fontFamily: "sans-serif"
  , fontSize: 15
  , align: relativePosition $ Relative LeftPos
  , verticalAlign: relativePosition $ Relative Top
  , lineHeight: percentPosition 0.0
  , width: percentPosition 100.0
  , height: percentPosition 100.0
  , textBorderColor: black
  , textBorderWidth: 1.0
  , textShadowColor: black
  , textShadowBlur: black
  , textShadowOffsetX: 0.0
  , textShadowOffsetY: 0.0
  }

textStyle :: TextStyle
textStyle =
  { color: black
  , fontStyle: chartFontStyle normal
  , fontWeight: chartFontWeight normal
  , fontFamily: "sans-serif"
  , fontSize: 15
  , align: relativePosition $ Relative LeftPos
  , verticalAlign: relativePosition $ Relative Top
  , lineHeight: percentPosition 0.0
  , width: percentPosition 100.0
  , height: percentPosition 100.0
  , textBorderColor: black
  , textBorderWidth: 1.0
  , textShadowColor: black
  , textShadowBlur: black
  , textShadowOffsetX: 0.0
  , textShadowOffsetY: 0.0
  }
