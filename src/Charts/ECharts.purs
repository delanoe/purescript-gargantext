module Charts.ECharts where


import CSS (black, blue, italic, violet, white, yellow)
import CSS.Common (normal)
import Charts.Series
import Charts.Data
import Charts.Color (chartColor)
import Charts.Font (IconOptions(..), Shape(..), TextStyle, chartFontStyle, chartFontWeight, icon)
import Charts.Legend (legendType, LegendMode(..), PlainOrScroll(..), selectedMode, Orientation(..), orient)
import Charts.Position (Align(..), LeftRelativePosition(..), TopRelativePosition(..), numberPosition, percentPosition, relativePosition)
import Charts.Type (DataZoom, Echarts, Legend, Option, Title, Tooltip, XAxis, YAxis)
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
 , show: true
 }

xAxis'' :: XAxis
xAxis'' =
 {
   "data": [xData1, xData2, xData3, xData4, xData5]
 , "type": "category"
 , axisTick: {alignWithLabel: true}
 , show: true
 }

xAxisVoid :: XAxis
xAxisVoid =
 {
   "data": []
 , "type": "category"
 , axisTick: {alignWithLabel: true}
 , show: false
 }

xData1 :: DataV
xData1 = {value: "Jan", textStyle: textStyle'}

xData2 :: DataV
xData2 = {value: "Feb", textStyle: textStyle'}

xData3 :: DataV
xData3 = {value: "Mar", textStyle: textStyle'}

xData4 :: DataV
xData4 = {value: "Apr", textStyle: textStyle'}

xData5 :: DataV
xData5 = {value: "May", textStyle: textStyle'}

xDataVoid :: DataV
xDataVoid = {value: "", textStyle: textStyle'}

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
  , position: "right"
  , axisLabel: {formatter: "{value}"}
  , show: true
  }

tooltip' :: Tooltip
tooltip' =
  {
    trigger: "axis"
  , formatter: Nothing
  }

seriesBar :: Series
seriesBar =
  {
    name: "Big Bar Data"
  , "type": seriesType Bar
  , "data": [{name: "Test1", value: 12.0},
             {name: "Test2", value: 20.0},
             {name: "Test4", value: 35.0},
             {name: "Test5", value: 2.0},
             {name: "Test3", value: 32.0}
             ]
  }

seriesHBar :: Series
seriesHBar =
  {
    name: "Funnel Data"
  , "type": seriesType Funnel
  , "data": [{name: "Test1", value: 60.0},
             {name: "Test2", value: 100.0},
             {name: "Test4", value: 40.0},
             {name: "Test5", value: 65.0},
             {name: "Test3", value: 32.0}
             ]
  }

seriesLine :: Series
seriesLine =
  {
    name: "Line Data"
  , "type": seriesType Line
  , "data": [{name: "Test1", value: 50.0},
             {name: "Test2", value: 45.0},
             {name: "Test3", value: 65.0},
             {name: "Test4", value: 15.0},
             {name: "Test5", value: 83.0}
             ]
  }

seriesPie :: Series
seriesPie =
  {
    name: "Pie"
  , "type": seriesType Pie
  , "data": [{name: "t1", value: 50.0},
             {name: "t2", value: 45.0},
             {name: "t3", value: 65.0},
             {name: "t4", value: 15.0},
             {name: "t5", value: 23.0}
             ]
  }

seriesWave :: Series
seriesWave =
  {
    name: "Bar Data"
  , "type": seriesType Bar
  , "data": [{name: "val1", value: 50.0},
             {name: "val2", value: 20.0},
             {name: "val5", value: 100.0}]
  }

optLineBar :: Option
optLineBar =
  {
    title: title
    ,legend: legend
    ,tooltip: tooltip'
    ,grid: {containLabel: true}
    ,xAxis: xAxis''
    ,yAxis: yData1
    ,series: [seriesBar, seriesLine]
    ,dataZoom: [dz1', dz1', dz2', dz2']
  }

optSunburst :: Option
optSunburst =
  {
    title: title
    ,legend: legend
    ,tooltip: tooltip'
    ,grid: {containLabel: true}
    ,xAxis: xAxisVoid
    ,yAxis: yDataVoid
    ,series: [seriesPie]
    ,dataZoom: []
  }

optHBar :: Option
optHBar =
  {
    title: title
    ,legend: legend
    ,tooltip: tooltip'
    ,grid: {containLabel: true}
    ,xAxis: xAxisVoid
    ,yAxis: yDataVoid
    ,series: [seriesHBar]
    ,dataZoom: []
  }

optWave :: Option
optWave =
  {
    title: title
    ,legend: legend
    ,tooltip: tooltip'
    ,grid: {containLabel: true}
    ,xAxis: xAxis'
    ,yAxis: yData1
    ,series: [seriesWave]
    ,dataZoom: []
  }

title :: Title
title =
  {
    id: ""
   ,show: true
   ,text: "Awesome Title"
   ,link: ""
   ,target: "blank"
   ,textStyle: textStyle
   ,subtext: "Awesome Subtitle"
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
   ,backgroundColor: chartColor white
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
    color: chartColor black
    ,fontStyle: chartFontStyle italic
    ,fontWeight: chartFontWeight normal
    ,fontFamily: "sans-serif"
    ,fontSize: 15
    ,align: relativePosition $ Relative RightPos
    ,verticalAlign: relativePosition $ Relative Bottom
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

textStyle' :: TextStyle
textStyle' =
  {
    color: chartColor black
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
    ,fontSize: 12
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

charts1 :: Echarts
charts1 =
  {
    className: Nothing
    ,style: Nothing
    ,theme: Nothing
    ,group: Nothing
    ,option: optLineBar
    ,initOpts: Nothing
    ,notMerge: Nothing
    ,lazyUpdate: Nothing
    ,loading: Nothing
    ,optsLoading: Nothing
    ,onReady: Nothing
    ,resizable: Nothing
    ,onEvents: Nothing
  }

charts2 :: Echarts
charts2 =
  {
    className: Nothing
    ,style: Nothing
    ,theme: Nothing
    ,group: Nothing
    ,option: optHBar
    ,initOpts: Nothing
    ,notMerge: Nothing
    ,lazyUpdate: Nothing
    ,loading: Nothing
    ,optsLoading: Nothing
    ,onReady: Nothing
    ,resizable: Nothing
    ,onEvents: Nothing
  }

charts3 :: Echarts
charts3 =
  {
    className: Nothing
    ,style: Nothing
    ,theme: Nothing
    ,group: Nothing
    ,option: optWave
    ,initOpts: Nothing
    ,notMerge: Nothing
    ,lazyUpdate: Nothing
    ,loading: Nothing
    ,optsLoading: Nothing
    ,onReady: Nothing
    ,resizable: Nothing
    ,onEvents: Nothing
  }

charts4 :: Echarts
charts4 =
  {
    className: Nothing
    ,style: Nothing
    ,theme: Nothing
    ,group: Nothing
    ,option: optSunburst
    ,initOpts: Nothing
    ,notMerge: Nothing
    ,lazyUpdate: Nothing
    ,loading: Nothing
    ,optsLoading: Nothing
    ,onReady: Nothing
    ,resizable: Nothing
    ,onEvents: Nothing
  }

histogram1 :: R.ReactElement
histogram1 = echarts charts1

histogram2 :: R.ReactElement
histogram2 = echarts charts2

histogram3 :: R.ReactElement
histogram3 = echarts charts3

histogram4 :: R.ReactElement
histogram4 = echarts charts4

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
