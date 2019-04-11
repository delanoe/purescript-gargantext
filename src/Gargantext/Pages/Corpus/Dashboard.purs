module Gargantext.Pages.Corpus.Dashboard where

import Prelude hiding (div)

import Data.Array (zipWith)
import Data.Tuple (Tuple(..))
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis', tooltipTriggerAxis)
import Gargantext.Components.Charts.Options.Data
import Gargantext.Components.Charts.Options.Series
import Data.Int (toNumber)
import React.DOM (div, h1, text)
import React.DOM.Props (className)
import Thermite (Render, Spec, simpleSpec, defaultPerformAction)

render :: Render {} {} Void
render dispatch _ state _ = [
    h1 [] [text "IMT DashBoard"]
    , div [className "row"] [ div [className "col-md-9 content"] [chart globalPublis]
                            , div [className "col-md-3 content"] [chart naturePublis]
                            ]
    , chart distriBySchool
    , div [className "row"] (map (\school -> div [className "col-md-4 content"] [chart $ focus school])
                                 [ "Télécom Bretagne", "Mines Nantes", "Eurecom"]
                            )
    , chart scatterEx
    , chart sankeyEx
    , chart treeMapEx
    , chart treeEx
  ]
    where
      myData = [seriesBarD1 {name: "Bar Data"}
                            [ dataSerie {name: "val1", value: 50.0}
                            , dataSerie {name: "val2", value: 70.0}
                            , dataSerie {name: "val3", value: 80.0}
                            ]
               ]

      focus :: String -> Options
      focus school = Options
        { mainTitle : "Focus " <> school
        , subTitle  : "Total scientific publications"
        , xAxis     : xAxis' ["2015", "2016", "2017"]
        , yAxis     : yAxis' { position: "left"
                             , show: false
                             }
        , series    : myData
        , addZoom   : false
        , tooltip   : tooltipTriggerAxis -- Necessary?
        }

-----------------------------------------------------------------------------------------------------------

naturePublis_x :: Array String
naturePublis_x = ["Com","Articles","Thèses","Reports"]
naturePublis_y' :: Array Int
naturePublis_y' = [23901,17417,1188,1176]

naturePublis_y :: Array DataD1
naturePublis_y = zipWith (\n v -> dataSerie {name: n, value: toNumber v }) naturePublis_x naturePublis_y'

naturePublis :: Options
naturePublis = Options
  { mainTitle : "Nature of publications"
  , subTitle  : "Distribution by type"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "left", show: false }
  , series    : [seriesFunnelD1 { name: "Funnel Data" } naturePublis_y]
  , addZoom   : false
  , tooltip   : tooltipTriggerAxis -- Necessary?
  }

-----------------------------------------------------------------------------------------------------------

globalPublis_x :: Array Int
globalPublis_x = [1982,1986,1987,1988,1990,1993,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017]
globalPublis_y :: Array Int
globalPublis_y = [1,4,2,1,1,2,1,1,8,38,234,76,40,82,75,202,1475,1092,1827,2630,4978,3668,4764,5915,4602,5269,6814,4018]


globalPublis :: Options
globalPublis = Options
  { mainTitle : "Histogram"
  , subTitle  : "Distribution of publications over time"
  , xAxis     : xAxis' (map show globalPublis_x)
  , yAxis     : yAxis' { position: "left", show: true }
  , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", value: toNumber n }) globalPublis_y]
  , addZoom   : true
  , tooltip   : tooltipTriggerAxis -- Necessary?
  }



distriBySchool_y :: Array (Tuple String Int)
distriBySchool_y = [Tuple "Télécom Bretagne" 1150,Tuple "Télécom SudParis" 946,Tuple "Mines Nantes" 547,Tuple "Télécom ParisTech" 429,Tuple "IMT Atlantique" 205,Tuple "Mines Alès" 56
                   ,Tuple "Télécom Ecole de Management" 52,Tuple "Mines Albi-Carmaux" 6]

distriBySchool :: Options
distriBySchool = Options
  { mainTitle : "School production in 2017"
  , subTitle  : "Distribution by school"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position : "", show: false }
  , series    : [ seriesPieD1 {name: "Pie data"} (map (\(Tuple n v) -> dataSerie {name: n, value: toNumber v}) distriBySchool_y)]
  , addZoom   : false
  , tooltip   : tooltipTriggerAxis -- Necessary?
  }

scatterEx :: Options
scatterEx = Options
  { mainTitle : "Scatter test"
  , subTitle  : "Scatter subtitle"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: true }
  , series    : [ seriesScatterD2 {name: "name1", symbolSize: 10.0} (dataSerieV <$> [[2.0,3.0],[3.0,4.0]])
                , seriesScatterD2 {name: "name2", symbolSize: 5.0 } (dataSerieV <$> [[1.0,3.0],[5.0,4.0]])
                , seriesScatterD2 {name: "name3", symbolSize: 10.0} (dataSerieV <$> [[10.0,3.0],[8.0,4.0]])
                ]
  , addZoom   : false
  , tooltip   : tooltipTriggerAxis -- Necessary?
  }

sankeyEx :: Options
sankeyEx = Options
  { mainTitle : ""
  , subTitle  : ""
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: false }
  , series    :
     [ seriesSankey
         { "data":
             [ {name : "a"}, {name : "b"}
             , {name:"c"}, {name:"d"} ]
         , links:
             [ {source : "a", target : "b", value :2.0}
             , {source : "a", target : "c", value :1.0}
             , {source : "b", target : "c", value :1.0}
             , {source : "b", target : "d", value :3.0}
             ]
         , layout: "none"
         }
     ]
  , tooltip   : tooltipTriggerAxis -- Necessary?
  , addZoom   : false
  }

treeData :: Array TreeData
treeData = [ treeNode "nodeA" 10.0 [ treeLeaf "nodeAa" 4.0
                                   , treeLeaf "nodeAb" 5.0
                                   , treeNode "nodeAc" 1.0 [ treeLeaf "nodeAca" 0.5
                                                           , treeLeaf "nodeAcb" 0.5
                                                          ]
                                   ]
           , treeNode "nodeB" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
           , treeNode "nodeC" 20.0 [ treeNode "nodeCa" 20.0 [ treeLeaf "nodeCa1" 10.0
                                                            , treeLeaf "nodeCa2" 10.0
                                                            ]
                                   ]
           , treeNode "nodeD" 20.0 [ treeNode "nodeDa" 20.0 [ treeLeaf "nodeDa1" 2.0
                                                            , treeLeaf "nodeDa2" 2.0
                                                            , treeLeaf "nodeDa3" 2.0
                                                            , treeLeaf "nodeDa4" 2.0
                                                            , treeLeaf "nodeDa5" 2.0
                                                            , treeLeaf "nodeDa6" 2.0
                                                            , treeLeaf "nodeDa7" 2.0
                                                            , treeLeaf "nodeDa8" 2.0
                                                            , treeLeaf "nodeDa9" 2.0
                                                            , treeLeaf "nodeDa10" 2.0
                                                            ]
                                     ]
          ]


treeData' :: Array TreeData
treeData' = [ treeNode "nodeA" 10.0 [ treeLeaf "nodeAa" 4.0
                                    , treeLeaf "nodeAb" 5.0
                                    , treeNode "nodeAc" 1.0 [ treeLeaf "nodeAca" 0.5
                                                           , treeLeaf "nodeAcb" 0.5
                                                          ]
                                   , treeNode "nodeB" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   , treeNode "nodeC" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   , treeNode "nodeD" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   , treeNode "nodeE" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   , treeNode "nodeF" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   , treeNode "nodeG" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   , treeNode "nodeH" 20.0 [ treeNode "nodeBa" 20.0 [ treeLeaf "nodeBa1" 20.0]]
                                   ]
          ]



treeMapEx :: Options
treeMapEx = Options
  { mainTitle : ""
  , subTitle  : ""
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: false }
  , series    : [mkTree TreeMap treeData]
  , addZoom   : false
  , tooltip   : tooltipTriggerAxis -- Necessary?
  }

treeEx :: Options
treeEx = Options
  { mainTitle : "Tree"
  , subTitle  : "Radial"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: false }
  , series    : [mkTree TreeRadial treeData']
  , addZoom   : false
  , tooltip   : tooltipTriggerAxis -- Necessary?
  }

layoutDashboard :: Spec {} {} Void
layoutDashboard = simpleSpec defaultPerformAction render
