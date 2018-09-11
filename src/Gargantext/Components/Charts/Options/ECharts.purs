module Gargantext.Components.Charts.Options.ECharts where

import Prelude

import CSS (black, italic, violet)
import CSS.Common (normal)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Gargantext.Components.Charts.Options.Color (chartColor, transparent)
import Gargantext.Components.Charts.Options.Data (DataN, DataS, DataV)
import Gargantext.Components.Charts.Options.Font (IconOptions(..), Shape(..), TextStyle, chartFontStyle, chartFontWeight, icon)
import Gargantext.Components.Charts.Options.Legend (legendType, LegendMode(..), PlainOrScroll(..), selectedMode, Orientation(..), orient)
import Gargantext.Components.Charts.Options.Position (Align(..), LeftRelativePosition(..), TopRelativePosition(..), numberPosition, percentPosition, relativePosition)
import Gargantext.Components.Charts.Options.Series (Serie(..), Series(..), toSeries, SeriesName, SeriesShape(..), seriesType, D1, D2)
import Gargantext.Components.Charts.Options.Type (DataZoom, Echarts, Legend, Option, Title, Tooltip, XAxis, YAxis)
import React (unsafeCreateElementDynamic)
import React as R
import Unsafe.Coerce (unsafeCoerce)


foreign import eChartsClass :: R.ReactClass Echarts

chart :: Options -> R.ReactElement
chart = echarts <<< chartWith <<< opts


chartWith :: Option -> Echarts
chartWith opts = { className : Nothing
                 , style     : Nothing
                 , theme     : Nothing
                 , group     : Nothing
                 , option    : opts
                 , initOpts  : Nothing
                 , notMerge  : Nothing
                 , lazyUpdate: Nothing
                 , loading   : Nothing
                 , optsLoading: Nothing
                 , onReady    : Nothing
                 , resizable  : Nothing
                 , onEvents   : Nothing
                 }

echarts :: Echarts -> R.ReactElement
echarts chart = unsafeCreateElementDynamic (unsafeCoerce eChartsClass) chart []

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
   ,left: relativePosition (Relative LeftPos)
   ,top: relativePosition (Relative Top)
   ,right: numberPosition 60.0
   ,bottom: percentPosition 40.0
   ,backgroundColor: chartColor transparent
   ,borderColor: chartColor transparent
   ,borderWidth: 0.0
   ,borderRadius: 0.0
   ,shadowBlur: 0.0
   ,shadowColor: chartColor transparent
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
   , formatter: Nothing
   , selectedMode: selectedMode $ Bool true
   , inactiveColor: chartColor violet
   , selected: Nothing
   , textStyle: textStyle
   , "data": [data1]
  }

data1 :: DataN
data1 = {name: "Map terms coverage", icon: icon $ Shape Circle, textStyle: textStyle'}

data2 :: DataN
data2 = {name: "Favorites", icon: icon $ Shape Circle, textStyle: textStyle'}

data3 :: DataN
data3 = {name: "Test", icon: icon $ Shape Diamond, textStyle: textStyle'}


xAxis :: Array String -> XAxis
xAxis [] = unsafeCoerce {}
xAxis xs = { "data": xData xs
           , "type": "category"
           , axisTick: {alignWithLabel: true}
           , show: if (length xs == 0) then false else true
           }
  where
    xData :: Array String -> Array DataV
    xData xs = map (\x -> {value : x, textStyle : textStyle'}) xs


yDataVoid :: YAxis
yDataVoid =
  {
    "type": ""
  , name: ""
  , min: 0
  , position: ""
  , axisLabel: {formatter: ""}
  , show: false
  }

yData1 :: YAxis
yData1 =
  {
    "type": "value"
  , name: "data"
  , min: 0
  , position: "left"
  , axisLabel: {formatter: "{value}"}
  , show: true
  }

tooltip' :: Tooltip
tooltip' =
  {
    trigger: "axis"
  , formatter: Nothing
  }


series :: SeriesShape -> SeriesName -> Array DataS -> D1
series sh name ss =  { name: name
  , "type": seriesType sh
  , "data": ss
  }

seriesD2 :: SeriesShape -> Number -> Array (Array Number) -> D2
seriesD2 sh size ds = { "symbolSize" : size
                      , "data"       : ds
                      , "type"       : seriesType sh
                    }



data YAxisFormat = YAxisFormat { position :: String
                               , visible  :: Boolean
                             }

data Options = Options { mainTitle   :: MainTitle
                       , subTitle    :: SubTitle
                       , xAxis       :: XAxis
                       , yAxis       :: Array Serie
                       , yAxisFormat :: YAxisFormat
                       , addZoom     :: Boolean
                     }

opts :: Options -> Option
opts (Options { mainTitle : mainTitle
              , subTitle : subTitle
              , xAxis : xs
              , yAxis : ss
              , yAxisFormat : (YAxisFormat { position : position
                                           , visible  : visible
                                         })
              , addZoom : addZoom}) =
  { title: title mainTitle subTitle
  , legend : legend
  , tooltip: { trigger: "axis"
             , formatter: Nothing
             }
  , grid   : {containLabel: true}
  , xAxis  : xs
  , series : map toSeries $ ss
  , yAxis  : { "type": "value"
              , name: "data"
              , min: 0
              , position: position
              , axisLabel: {formatter: "{value}"}
              , show: visible
              }
  , dataZoom: if addZoom then [zoom Slider, zoom Inside] else []
  , children : unsafeCoerce []
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


seriesPie :: D1
seriesPie =
  {
    name: "Pie name"
  , "type": seriesType Pie
  , "data": [{name: "t1", value: 50.0},
             {name: "t2", value: 45.0},
             {name: "t3", value: 65.0},
             {name: "t4", value: 15.0},
             {name: "t5", value: 23.0}
             ]
  }


textStyle2 :: TextStyle
textStyle2 =
  {
    color: chartColor black
    ,fontStyle: chartFontStyle italic
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 12
    ,align: relativePosition $ Relative RightPos
    ,verticalAlign: relativePosition $ Relative Bottom
    ,lineHeight: percentPosition 0.0
    ,width: percentPosition 100.0
    ,height: percentPosition 100.0
    ,textBorderColor: chartColor black
    ,textBorderWidth: 0.0
    ,textShadowColor: chartColor black
    ,textShadowBlur: chartColor black
    ,textShadowOffsetX: 0.0
    ,textShadowOffsetY: 0.0
  }

textStyle' :: TextStyle
textStyle' =
  {
    color: chartColor black
    ,fontStyle: chartFontStyle normal
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 15
    ,align: relativePosition $ Relative LeftPos
    ,verticalAlign: relativePosition $ Relative Top
    ,lineHeight: percentPosition 0.0
    ,width: percentPosition 100.0
    ,height: percentPosition 100.0
    ,textBorderColor: chartColor black
    ,textBorderWidth: 1.0
    ,textShadowColor: chartColor black
    ,textShadowBlur: chartColor black
    ,textShadowOffsetX: 0.0
    ,textShadowOffsetY: 0.0
  }

textStyle :: TextStyle
textStyle =
  {
    color: chartColor black
    ,fontStyle: chartFontStyle normal
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 20
    ,align: relativePosition $ Relative LeftPos
    ,verticalAlign: relativePosition $ Relative Top
    ,lineHeight: percentPosition 0.0
    ,width: percentPosition 100.0
    ,height: percentPosition 100.0
    ,textBorderColor: chartColor black
    ,textBorderWidth: 1.0
    ,textShadowColor: chartColor black
    ,textShadowBlur: chartColor black
    ,textShadowOffsetX: 0.0
    ,textShadowOffsetY: 0.0
  }
