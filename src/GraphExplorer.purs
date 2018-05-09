module GraphExplorer where

import React.DOM (button, button', div, form', input, li', menu, text, ul, ul')
import React.DOM.Props (_id, _type, className, name, placeholder, value)
import Thermite (Spec, defaultPerformAction, simpleSpec)
import Sigma

newtype State = State {mode :: String}

data Action = NoOp

initialState :: State
initialState = State {mode : "select"}

spec :: forall eff props. Spec eff State props Action
spec = simpleSpec defaultPerformAction render
  where
    render _ _ _ _ =
      [  div [className "row"] [
            div [className "col-md-12"]
            [ menu [_id "toolbar"]
              [ ul'
                [ li'
                  [ form'
                    [ input [_type "file", name "file", value ""] []
                    , input [_type "submit", value "submit"] []
                    ]
                  ]
                , li'
                  [ button [className "btn btn-success btn-sm"] [text "Change Type"]
                  ]
                , li'
                  [ button [className "btn btn-primary btn-sm"] [text "Change Level"]
                  ]
                , li'
                  [ form'
                    [ input [_type "text", name "query", placeholder "Select Topics"] []
                    , input [_type "submit", value "Search"] []
                    ]
                  ]
                , li'
                  [ button' [text "Screenshot"]]
                , li'
                  [ button' [text "Save"] -- TODO: Implement Save!
                  ]
                ]
              ]
            ]
           ]
         , div [className "row"]
           [ div [className "col-md-8"]
             [ --div [] [text "GraphExplorer here...."]
               sigmaC [ style {width : 500, height : 500}, renderer "webgl"]
               [ loadJSON [path "./sites_coords.json"]
               ]
             ]
           , div [className "col-md-4"]
             [ div [_id "sidepanel"]
               [ text "SidePanel for contextual information"
               ]
             ]
           ]
         ]
