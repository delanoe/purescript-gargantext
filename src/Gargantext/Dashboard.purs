module Gargantext.Dashboard where

import Prelude (($), (<>), show, pure, unit, map)
import Gargantext.Charts.ECharts
import Gargantext.Charts.Series

import DOM (DOM)
import Data.Unit (Unit)
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
                                 [ "Telecom Paris Sud", "Telecom Bretagne", "Telecom ParisTech"]
                            )
  ]
    where
      globalPublis :: Options
      globalPublis = (Options { mainTitle : "Global Scientific Publications"
                           , subTitle  : "Distribution of scientific publications by IMT's Schools over time"
                           , xAxis     : xAxis ["Jan", "Feb", "Mar", "Apr", "May"]
                           , yAxis     : [series Bar "Number of publication of IMT / year" [ {name: "Test1", value: 12.0}
                                                              , {name: "Test2", value: 20.0}
                                                              , {name: "Test4", value: 35.0}
                                                              , {name: "Test5", value: 2.0}
                                                              , {name: "Test3", value: 32.0}
                                                              ]
                                         ]
                           , yAxisFormat : (YAxisFormat { position : "left"
                                                        , visible  : true
                                                      })
                           , addZoom  : true
                         })
      distriBySchool :: Options
      distriBySchool = Options { mainTitle : "School production in 2018"
                             , subTitle  : "Distribution by school"
                             , xAxis     : xAxis []
                             , yAxis     : [ series Pie "Pie data" [{name: "Sud Paris", value: 50.0},
                                                                    {name: "Eurecom", value: 45.0},
                                                                    {name: "Telecom ParisTech", value: 65.0},
                                                                    {name: "Telecom Bretagne", value: 15.0},
                                                                    {name: "Telecom Saint-Etienne", value: 23.0}
                                                                    ]
                                           ]
                             , yAxisFormat : (YAxisFormat { position : ""
                                                          , visible  : false
                                                        })
                             , addZoom     : false
                           }

      focus :: String -> Options
      focus school = Options { mainTitle : ("Focus " <> school)
                         , subTitle  : "Total scientific publications"
                         , xAxis     : xAxis ["Jan", "Feb", "Mar"]
                         , yAxis     : [series Bar "Bar Data"  [ {name: "val1", value: 50.0}
                                           , {name: "val2", value: 20.0}
                                           , {name: "val5", value: 100.0}
                                           ]
                                           ]
                         , yAxisFormat : (YAxisFormat { position : "left"
                                                      , visible  : true
                                                      })

                         , addZoom   : false
                         }


      naturePublis :: Options
      naturePublis = Options { mainTitle : "Nature of publications"
                        , subTitle  : "Distribution by type"
                        , xAxis     : xAxis []
                        , yAxis     : [series Funnel "Funnel Data" [ {name: "Articles", value: 60.0}
                                               , {name: "Reports", value: 100.0}
                                               , {name: "Patents", value: 40.0}
                                               , {name: "Books", value: 65.0}
                                               ]
                                       ]
                        , yAxisFormat : (YAxisFormat { position : "left"
                                                     , visible  : false
                                                   })
                        , addZoom   : false
                      }



layoutDashboard :: forall props eff. Spec (dom :: DOM | eff) State props Action
layoutDashboard = simpleSpec performAction render
