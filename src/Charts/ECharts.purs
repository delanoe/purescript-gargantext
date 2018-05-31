module Charts.ECharts where


import CSS (black, blue, italic, violet, white, yellow)
import CSS.Common (normal)
import Charts.Data
import Charts.Color (chartColor)
import Charts.Font (IconOptions(..), Shape(..), TextStyle, chartFontStyle, chartFontWeight, icon)
import Charts.Legend (legendType, LegendMode(..), PlainOrScroll(..), selectedMode, Orientation(..), orient)
import Charts.Position (Align(..), LeftRelativePosition(..), TopRelativePosition(..), numberPosition, percentPosition, relativePosition)
import Charts.Type (DataZoom, Echarts, Legend, Option, Series, Title, Tooltip, XAxis, YAxis)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (($))
import React as R
import React.DOM (p)

foreign import eChartsClass :: R.ReactClass Echarts

echarts :: forall eff. Echarts -> R.ReactElement
echarts chart = R.createElementDynamic eChartsClass chart []

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

xAxis' :: XAxis
xAxis' =
 {
   "data": [xData1, xData2, xData3]
 , "type": "category"
 , axisTick: {alignWithLabel: true}
 }

xData1 :: DataV
xData1 = {value: "Jan", textStyle: textStyle'}

xData2 :: DataV
xData2 = {value: "Feb", textStyle: textStyle'}

xData3 :: DataV
xData3 = {value: "Mar", textStyle: textStyle'}

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
    ,legend: legend
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
    id: "Muda"
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
   ,left: relativePosition (Relative LeftPos)
   ,top: relativePosition (Relative Top)
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
    ,fontStyle: chartFontStyle italic
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

textStyle' :: TextStyle
textStyle' =
  {
    color: chartColor violet
    ,fontStyle: chartFontStyle normal
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 12
    ,align: relativePosition $ Relative LeftPos
    ,verticalAlign: relativePosition $ Relative Top
    ,lineHeight: percentPosition 0.0
    ,width: percentPosition 100.0
    ,height: percentPosition 100.0
    ,textBorderColor: chartColor black
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
