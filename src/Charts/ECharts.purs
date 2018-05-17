module Charts.ECharts where

import CSS (Color)
import Data.Maybe (Maybe(..))
import React as R
import React.DOM (p)
import React.DOM.Props (Props)
import Charts.Types

foreign import eChartsClass :: forall props. R.ReactClass props
foreign import eChartsClass2 :: R.ReactClass Echarts

echarts' :: forall eff. Echarts -> R.ReactElement
echarts' chart = R.createElementDynamic eChartsClass2 chart []

legend' :: Legend
legend' =
  {
    "type": "plain"
   , show: true
   , zlevel: Nothing
   , z: Nothing
   , left: Nothing
   , top: Nothing
   , right: Nothing
   , bottom: Nothing
   , width: Nothing
   , height: Nothing
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
    title: Nothing
    ,legend: Nothing
    ,tooltip: tooltip'
    ,grid: {containLabel: true}
    ,xAxis: xAxis'
    ,yAxis: yData1
    ,series: [series']
    ,dataZoom: [dz1', dz1', dz2', dz2']
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
histogram2 = echarts' charts

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
     ]

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
