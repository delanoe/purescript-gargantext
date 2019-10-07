module Gargantext.Pages.Corpus.Dashboard where

import Prelude (map, show, ($), (<$>), (<>))
import Data.Array (zipWith)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis', tooltipTriggerAxis)
import Gargantext.Components.Charts.Options.Data
import Gargantext.Components.Charts.Options.Series
  ( TreeNode
  , Trees(..)
  , mkTree
  , seriesBarD1
  , seriesFunnelD1
  , seriesPieD1
  , seriesSankey
  , seriesScatterD2
  , treeLeaf
  , treeNode
  )

dashboardLayout :: {} -> R.Element
dashboardLayout props = R.createElement dashboardLayoutCpt props []

dashboardLayoutCpt :: R.Component ()
dashboardLayoutCpt = R.staticComponent "G.P.Corpus.Dashboard.dashboardLayout" cpt
  where
  cpt _ _ =
    R.fragment
      [ H.h1 {} [ H.text "IMT DashBoard" ]
      , H.div { className: "row" }
          [ H.div { className: "col-md-9 content" } [ chart globalPublis ]
          , H.div { className: "col-md-3 content" } [ chart naturePublis ]
          ]
      , chart distriBySchool
      , H.div { className: "row" } (aSchool <$> schools)
      , chart scatterEx
      , chart sankeyEx
      , chart treeMapEx
      , chart treeEx
      ]

  aSchool school = H.div { className: "col-md-4 content" } [ chart $ focus school ]

  schools = [ "Télécom Bretagne", "Mines Nantes", "Eurecom" ]

  myData =
    [ seriesBarD1 { name: "Bar Data" }
        [ dataSerie { name: "val1", value: 50.0 }
        , dataSerie { name: "val2", value: 70.0 }
        , dataSerie { name: "val3", value: 80.0 }
        ]
    ]

  focus :: String -> Options
  focus school =
    Options
      { mainTitle: "Focus " <> school
      , subTitle: "Total scientific publications"
      , xAxis: xAxis' [ "2015", "2016", "2017" ]
      , yAxis: yAxis' { position: "left", show: false, min: 0 }
      , series: myData
      , addZoom: false
      , tooltip: tooltipTriggerAxis
      } -- Necessary?

-----------------------------------------------------------------------------------------------------------
naturePublis_x :: Array String
naturePublis_x = [ "Com", "Articles", "Thèses", "Reports" ]

naturePublis_y' :: Array Int
naturePublis_y' = [ 23901, 17417, 1188, 1176 ]

naturePublis_y :: Array DataD1
naturePublis_y = zipWith (\n v -> dataSerie { name: n, value: toNumber v }) naturePublis_x naturePublis_y'

naturePublis :: Options
naturePublis =
  Options
    { mainTitle: "Nature of publications"
    , subTitle: "Distribution by type"
    , xAxis: xAxis' []
    , yAxis: yAxis' { position: "left", show: false, min: 0 }
    , series: [ seriesFunnelD1 { name: "Funnel Data" } naturePublis_y ]
    , addZoom: false
    , tooltip: tooltipTriggerAxis -- Necessary?
    }

-----------------------------------------------------------------------------------------------------------
globalPublis_x :: Array Int
globalPublis_x = [ 1982, 1986, 1987, 1988, 1990, 1993, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 ]

globalPublis_y :: Array Int
globalPublis_y = [ 1, 4, 2, 1, 1, 2, 1, 1, 8, 38, 234, 76, 40, 82, 75, 202, 1475, 1092, 1827, 2630, 4978, 3668, 4764, 5915, 4602, 5269, 6814, 4018 ]

globalPublis :: Options
globalPublis =
  Options
    { mainTitle: "Histogram"
    , subTitle: "Distribution of publications over time"
    , xAxis: xAxis' (map show globalPublis_x)
    , yAxis: yAxis' { position: "left", show: true, min: 0 }
    , series: [ seriesBarD1 { name: "Number of publication / year" } $ map (\n -> dataSerie { name: "", value: toNumber n }) globalPublis_y ]
    , addZoom: true
    , tooltip: tooltipTriggerAxis -- Necessary?
    }

distriBySchool_y :: Array (Tuple String Int)
distriBySchool_y =
  [ Tuple "Télécom Bretagne" 1150
  , Tuple "Télécom SudParis" 946
  , Tuple "Mines Nantes" 547
  , Tuple "Télécom ParisTech" 429
  , Tuple "IMT Atlantique" 205
  , Tuple "Mines Alès" 56
  , Tuple "Télécom Ecole de Management" 52
  , Tuple "Mines Albi-Carmaux" 6
  ]

distriBySchool :: Options
distriBySchool =
  Options
    { mainTitle: "School production in 2017"
    , subTitle: "Distribution by school"
    , xAxis: xAxis' []
    , yAxis: yAxis' { position: "", show: false, min: 0 }
    , series: [ seriesPieD1 { name: "Pie data" } (map (\(Tuple n v) -> dataSerie { name: n, value: toNumber v }) distriBySchool_y) ]
    , addZoom: false
    , tooltip: tooltipTriggerAxis -- Necessary?
    }

scatterEx :: Options
scatterEx =
  Options
    { mainTitle: "Scatter test"
    , subTitle: "Scatter subtitle"
    , xAxis: xAxis' []
    , yAxis: yAxis' { position: "", show: true, min: 0 }
    , series:
      [ seriesScatterD2 { name: "name1", symbolSize: 10.0 } (dataSerieV <$> [ [ 2.0, 3.0 ], [ 3.0, 4.0 ] ])
      , seriesScatterD2 { name: "name2", symbolSize: 5.0 } (dataSerieV <$> [ [ 1.0, 3.0 ], [ 5.0, 4.0 ] ])
      , seriesScatterD2 { name: "name3", symbolSize: 10.0 } (dataSerieV <$> [ [ 10.0, 3.0 ], [ 8.0, 4.0 ] ])
      ]
    , addZoom: false
    , tooltip: tooltipTriggerAxis -- Necessary?
    }

sankeyEx :: Options
sankeyEx =
  Options
    { mainTitle: ""
    , subTitle: ""
    , xAxis: xAxis' []
    , yAxis: yAxis' { position: "", show: false, min: 0 }
    , series:
      [ seriesSankey
          { "data":
            [ { name: "a" }
            , { name: "b" }
            , { name: "c" }
            , { name: "d" }
            ]
          , links:
            [ { source: "a", target: "b", value: 2.0 }
            , { source: "a", target: "c", value: 1.0 }
            , { source: "b", target: "c", value: 1.0 }
            , { source: "b", target: "d", value: 3.0 }
            ]
          , layout: "none"
          }
      ]
    , tooltip: tooltipTriggerAxis -- Necessary?
    , addZoom: false
    }

treeData :: Array TreeNode
treeData =
  [ treeNode "nodeA" 10
      [ treeNode "nodeAa" 4 []
      , treeNode "nodeAb" 5 []
      , treeNode "nodeAc" 1
          [ treeNode "nodeAca" 5 []
          , treeNode "nodeAcb" 5 []
          ]
      ]
  , treeNode "nodeB" 20
      [ treeNode "nodeBa" 20
          [ treeNode "nodeBa1" 20 [] ]
      ]
  , treeNode "nodeC" 20
      [ treeNode "nodeCa" 20
          [ treeNode "nodeCa1" 10 []
          , treeNode "nodeCa2" 10 []
          ]
      , treeNode "nodeD" 20
          [ treeNode "nodeDa" 20
              [ treeNode "nodeDa1" 2 []
              , treeNode "nodeDa2" 2 []
              , treeNode "nodeDa3" 2 []
              , treeNode "nodeDa4" 2 []
              , treeNode "nodeDa5" 2 []
              , treeNode "nodeDa6" 2 []
              , treeNode "nodeDa7" 2 []
              , treeNode "nodeDa8" 2 []
              , treeNode "nodeDa9" 2 []
              , treeNode "nodeDa10" 2 []
              ]
          ]
      ]
  ]

treeData' :: Array TreeNode
treeData' =
  [ treeNode "nodeA" 10
      [ treeLeaf "nodeAa" 4
      , treeLeaf "nodeAb" 5
      , treeNode "nodeAc" 1 [ treeLeaf "nodeAca" 5, treeLeaf "nodeAcb" 5 ]
      ]
  , treeNode "nodeB" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  , treeNode "nodeC" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  , treeNode "nodeD" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  , treeNode "nodeE" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  , treeNode "nodeF" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  , treeNode "nodeG" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  , treeNode "nodeH" 20 [ treeNode "nodeBa" 20 [ treeLeaf "nodeBa1" 20 ] ]
  ]

treeMapEx :: Options
treeMapEx =
  Options
    { mainTitle: ""
    , subTitle: ""
    , xAxis: xAxis' []
    , yAxis: yAxis' { position: "", show: false, min: 0 }
    , series: [ mkTree TreeMap treeData ]
    , addZoom: false
    , tooltip: tooltipTriggerAxis -- Necessary?
    }

treeEx :: Options
treeEx =
  Options
    { mainTitle: "Tree"
    , subTitle: "Radial"
    , xAxis: xAxis' []
    , yAxis: yAxis' { position: "", show: false, min: 0 }
    , series: [ mkTree TreeRadial treeData' ]
    , addZoom: false
    , tooltip: tooltipTriggerAxis -- Necessary?
    }
