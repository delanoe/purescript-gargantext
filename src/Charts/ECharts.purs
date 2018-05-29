module Charts.ECharts where

import CSS (black, blue, white, yellow)
import CSS.Common (normal)
import Charts.Color (chartColor)
import Charts.Font (chartFontStyle, chartFontWeight)
import Charts.Legend (legendType, LegendType, PlainOrScroll(..))
import Charts.Position (Align(..), LeftRelativePosition(..), TopRelativePosition(..), numberPosition, percentPosition, relativePosition)
import Charts.Type (Data, DataZoom, Echarts, Legend, Option, Series, TextStyle, Title, Tooltip, XAxis, YAxis)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (($))
import React as R
import React.DOM (p)

foreign import eChartsClass :: R.ReactClass Echarts

echarts :: forall eff. Echarts -> R.ReactElement
echarts chart = R.createElementDynamic eChartsClass chart []

legend' :: Legend
legend' =
  {
    id: ""
   ,"type": legendType Plain
   , show: true
   , zlevel: 40.0
   , z: 40.0
   , left: percentPosition 40.0
   , top:  percentPosition 40.0
   , right:  percentPosition 40.0
   , bottom:  percentPosition 40.0
   , width: percentPosition 40.0
   , height: percentPosition 40.0
   , orient: Nothing
   , align: Nothing
   , padding: Nothing
   , itemGap: Nothing
   , itemWidth: Nothing
   , itemHeight: Nothing
   , formatter: Nothing
   , selectedMode: Nothing
   , inactiveColor: Nothing
   , selected: Nothing
   , "data": Nothing
  }

data1 :: Data
data1 = {name: "Map terms coverage", icon: Nothing, textStyle: Nothing}

data2 :: Data
data2 = {name: "Favorites", icon: Nothing, textStyle: Nothing}

data3 :: Data
data3 = {name: "All", icon: Nothing, textStyle: Nothing}

xAxis' :: XAxis
xAxis' =
 {
   "data": [xData1, xData2, xData3]
 , "type": "category"
 , axisTick: {alignWithLabel: true}
 }

xData1 :: Data
xData1 = {name: "Jan", icon: Nothing, textStyle: Nothing}

xData2 :: Data
xData2 = {name: "Feb", icon: Nothing, textStyle: Nothing}

xData3 :: Data
xData3 = {name: "Mar", icon: Nothing, textStyle: Nothing}

yData1 :: YAxis
yData1 =
  {
    "type": "value"
  , name: "Score metric"
  , min: 0
  , position: "right"
  , axisLabel: {formatter: "{value}"}
  }

tooltip' :: Tooltip
tooltip' =
  {
    trigger: "axis"
  , formatter: Nothing
  }


series' :: Series
series' =
  {
    name: "All"
  , "type": "bar"
  , "data": [201, 777, 879]
  }

opt :: Option
opt =
  {
    title: title
    ,legend: Nothing
    ,tooltip: tooltip'
    ,grid: {containLabel: true}
    ,xAxis: xAxis'
    ,yAxis: yData1
    ,series: [series']
    ,dataZoom: [dz1', dz1', dz2', dz2']
  }

title :: Title
title =
  {
    id: ""
   ,show: true
   ,text: "MudaTitre rpz les pyramides"
   ,link: "https://google.com"
   ,target: "blank"
   ,textStyle: textStyle
   ,subtext: "Muda Subtitle"
   ,sublink: "https://google.fr"
   ,subtarget: "blank"
   ,subtextStyle: textStyle2
   ,padding: 10.0
   ,itemGap: 0.0
   ,zlevel: 2.0
   ,z: 2.0
   ,left: relativePosition (Relative Center)
   ,top: relativePosition (Relative Middle)
   ,right: numberPosition 60.0
   ,bottom: percentPosition 40.0
   ,backgroundColor: chartColor black
   ,borderColor: chartColor black
   ,borderWidth: 0.0
   ,borderRadius: 20.0
   ,shadowBlur: 0.0
   ,shadowColor: chartColor black
   ,shadowOffsetX: 0.0
   ,shadowOffsetY: 0.0
  }

textStyle2 :: TextStyle
textStyle2 =
  {
    color: chartColor yellow
    ,fontStyle: chartFontStyle normal
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 12
    ,align: relativePosition $ Relative RightPos
    ,verticalAlign: relativePosition $ Relative Bottom
    ,lineHeight: percentPosition 0.0
    ,width: percentPosition 100.0
    ,height: percentPosition 100.0
    ,textBorderColor: chartColor blue
    ,textBorderWidth: 5.0
    ,textShadowColor: chartColor black
    ,textShadowBlur: chartColor black
    ,textShadowOffsetX: 0.0
    ,textShadowOffsetY: 0.0
  }


textStyle :: TextStyle
textStyle =
  {
    color: chartColor white
    ,fontStyle: chartFontStyle normal
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 12
    ,align: relativePosition $ Relative LeftPos
    ,verticalAlign: relativePosition $ Relative Top
    ,lineHeight: percentPosition 0.0
    ,width: percentPosition 100.0
    ,height: percentPosition 100.0
    ,textBorderColor: chartColor blue
    ,textBorderWidth: 5.0
    ,textShadowColor: chartColor black
    ,textShadowBlur: chartColor black
    ,textShadowOffsetX: 0.0
    ,textShadowOffsetY: 0.0
  }

charts :: Echarts
charts =
  {
    className: Nothing
    ,style: Nothing
    ,theme: Nothing
    ,group: Nothing
    ,option: opt
    ,initOpts: Nothing
    ,notMerge: Nothing
    ,lazyUpdate: Nothing
    ,loading: Nothing
    ,optsLoading: Nothing
    ,onReady: Nothing
    ,resizable: Nothing
    ,onEvents: Nothing
  }

histogram2 :: R.ReactElement
histogram2 = echarts charts

{-
histogram :: R.ReactElement
histogram = echarts
     [ option
       [ tooltip [trigger "axis"]
       , grid [containLabel true]
       , legend [data' ["TEST MUDADA", "Favorites", "All"]]
       -- , legend [data' ["Map Terms coverage", "Favorites", "All"]]
       , xAxis
         [ type' "category"
         , axisTick [alignWithLabel true]
         , data' ["Jan" , "Feb", "Mar" , "Apr"
                 , "May", "Jun", "July", "Aug"
                 , "Sep", "Oct", "Nov" , "Dec"
                 ]
         ]
       , dataZoom' [dz1', dz1', dz2', dz2']
       , yAxis [ya1, ya2]
       , series [sd1, sd2, sd3]
       ]
j    ]

type DataZoom =
  {"type"      :: String
  , xAxisIndex :: Int
  , filterMode :: String
  , start      :: Int
  , end        :: Int
  }
-}

dz1' :: DataZoom
dz1' = {
  "type": "slider"
  ,xAxisIndex: 0
  ,filterMode: "empty"
  ,start: 0
  ,end: 100
  }

dz2' :: DataZoom
dz2' = {
  "type": "inside"
  ,xAxisIndex: 0
  ,filterMode: "empty"
  ,start: 0
  ,end: 100
  }

p'' :: R.ReactElement
p'' = p [] []
