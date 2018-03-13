module UserPage where

import Prelude hiding (div)

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import React.DOM (a, div, h3, h5, img, span, text)
import React.DOM.Props (_id, className, src)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String



initialState :: State
initialState =""

data Action
  = NoOp


performAction :: forall eff props. PerformAction (console :: CONSOLE, ajax :: AJAX,dom::DOM | eff) State props Action
performAction NoOp _ _ = void do
  modifyState id



userPageSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
userPageSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ div [className "container-fluid"]
        [ div [className "row", _id "user-page-header"]
          [ div [className "col-md-2"]
            [ h3 [] [text "UserName"]
            ]
          , div [className "col-md-8"] []
          , div [className "col-md-2"]
            [ span [] [text "X"]
            ]
          ]
        , div [className "row", _id "user-page-info"]
          [
            div [className "col-md-12"]
            [ div [className "row"]
              [ div [className "col-md-2"]
                [ img [src "/images/corporate.jpg"] []
                ]
              , div [className "col-md-8"]
                [
                  div [className "list-group"]
                  [
                    a [className "list-group-item list-group-item-action flex-column align-items-start"]
                    [ div [className "d-flex w-100 justify-content-between"]
                      [ h5 [className "mb-1"] [ text "fonction"]
                      , h5 [] [text "Enseignent checheur"]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        , div [className "row",_id "user-page-footer"]
          [
          ]
        ]
      ]
