module Gargantext.Dashboard where

import Prelude (($), (<>), show, pure, unit, map)
import Data.Array (zip)
import Data.Tuple (Tuple(..))
import Gargantext.Charts.ECharts
import Gargantext.Charts.Series

import DOM (DOM)
import Data.Unit (Unit)
import Data.Int (toNumber)

import React.DOM (div, h1, text, title)
import React.DOM.Props (className)
import Thermite (PerformAction, Render, Spec, simpleSpec)

type State = Unit

data Action = None

initialState :: State
initialState = unit

performAction :: forall eff props. PerformAction (dom :: DOM | eff) State props Action
performAction _ _ _ = pure unit

render :: forall props. Render State props Action
render dispatch _ state _ = [
    h1 [] [text "IMT DashBoard"]
    , div [className "row"] [ div [className "col-md-9 content"] [chart globalPublis]
                            , div [className "col-md-3 content"] [chart naturePublis]
                            ]
    , chart distriBySchool
    , div [className "row"] (map (\school -> div [className "col-md-4 content"] [chart $ focus school])
                                 [ "Télécom Bretagne", "Mines Nantes", "Eurecom"]
                            )
  ]
    where

      focus :: String -> Options
      focus school = Options { mainTitle : ("Focus " <> school)
                         , subTitle  : "Total scientific publications"
                         , xAxis     : xAxis ["2015", "2016", "2017"]
                         , yAxis     : [series Bar "Bar Data"  [ {name: "val1", value: 50.0}
                                           , {name: "val2", value: 70.0}
                                           , {name: "val3", value: 80.0}
                                           ]
                                           ]
                         , yAxisFormat : (YAxisFormat { position : "left"
                                                      , visible  : true
                                                      })

                         , addZoom   : false
                         }


-----------------------------------------------------------------------------------------------------------

naturePublis_x = ["Com","Articles","Thèses","Reports"]
naturePublis_y' = [23901,17417,1188,1176]

naturePublis_y = map (\(Tuple n v) -> {name: n, value: toNumber v }) (zip naturePublis_x naturePublis_y')

naturePublis :: Options
naturePublis = Options { mainTitle : "Nature of publications"
                  , subTitle  : "Distribution by type"
                  , xAxis     : xAxis []
                  , yAxis     : [series Funnel "Funnel Data" naturePublis_y]
                  , yAxisFormat : (YAxisFormat { position : "left"
                                               , visible  : false
                                             })
                  , addZoom   : false
                }

-----------------------------------------------------------------------------------------------------------

globalPublis_x = [1982,1986,1987,1988,1990,1993,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017]
globalPublis_y = [1,4,2,1,1,2,1,1,8,38,234,76,40,82,75,202,1475,1092,1827,2630,4978,3668,4764,5915,4602,5269,6814,4018]


globalPublis :: Options
globalPublis = (Options { mainTitle : "Global Scientific Publications"
                     , subTitle  : "Distribution of scientific publications by IMT's Schools over time"
                     , xAxis     : xAxis (map show globalPublis_x)
                     , yAxis     : [series Bar "Number of publication of IMT / year" $ map (\n -> {name: "", value: toNumber n }) globalPublis_y]
                     , yAxisFormat : (YAxisFormat { position : "left"
                                                  , visible  : true
                                                })
                     , addZoom  : true
                   })



distriBySchool_y = [(Tuple "Télécom Bretagne" 17383),(Tuple "Mines Nantes" 11055),(Tuple "Télécom SudParis" 7663),(Tuple "Télécom ParisTech" 3848),(Tuple "Mines Al\232s" 624),(Tuple "Télécom \201cole de Management" 539),(Tuple "IMT Atlantique" 241),(Tuple "Mines Albi-Carmaux" 125)]

distriBySchool :: Options
distriBySchool = Options { mainTitle : "School production in 2018"
                       , subTitle  : "Distribution by school"
                       , xAxis     : xAxis []
                       , yAxis     : [ series Pie "Pie data" (map (\(Tuple n v) -> {name: n, value: toNumber v}) distriBySchool_y)]
                       , yAxisFormat : (YAxisFormat { position : ""
                                                    , visible  : false
                                                  })
                       , addZoom     : false
                     }


layoutDashboard :: forall props eff. Spec (dom :: DOM | eff) State props Action
layoutDashboard = simpleSpec performAction render
