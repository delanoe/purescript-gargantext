module Dashboard where

import Charts.ECharts

import DOM (DOM)
import Data.Unit (Unit)
import Prelude (pure, unit)
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
    h1 [] [text "DashBoard"]
  , histogram1
  , div [className "row"] [
      div [className "col-md-4 content"] [histogram2]
    , div [className "col-md-4 content"] [histogram3]
    , div [className "col-md-4 content"] [histogram4]
    ]
  ]

layoutDashboard :: forall props eff. Spec (dom :: DOM | eff) State props Action
layoutDashboard = simpleSpec performAction render
