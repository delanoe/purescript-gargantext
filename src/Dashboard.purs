module Dashboard where

import DOM (DOM)
import Data.Unit (Unit)
import Prelude (pure, unit)
import React.DOM (text)
import Thermite (PerformAction, Render, Spec, simpleSpec)
import Charts.ECharts

type State = Unit

data Action = None

initialState :: State
initialState = unit

performAction :: forall eff props. PerformAction (dom :: DOM | eff) State props Action
performAction _ _ _ = pure unit

render :: forall props. Render State props Action
render dispatch _ state _ = [text "Dashboard", histogram2]

layoutDashboard :: forall props eff. Spec (dom :: DOM | eff) State props Action
layoutDashboard = simpleSpec performAction render
