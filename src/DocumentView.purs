module DocumentView where

import Prelude hiding (div)
import React.DOM (div, h4, text)
import React.DOM.Props (className)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""


data Action = NoOp


performAction :: PerformAction _ State _ Action
performAction NoOp _ _ = pure unit



docview :: Spec _ State _ Action
docview = simpleSpec performAction render
  where
    render :: Render State _ Action
    render dispatch _ state _ =
      [ div [className "container"]
        [ div [className "row"]
          [ div [className "col-md-4"] []
          , div [className "col-md-8"]
            [ h4 [] [text "Ultrasonic sensors in urban traffic driving-aid systems"]
            ]
          ]
        ]
      ]
