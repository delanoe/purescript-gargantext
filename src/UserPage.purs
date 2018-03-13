module UserPage where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, div, h3, h5, h6, img, li, small, span, text, ul)
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
                [ img [src "/images/Gargantextuel-212x300.jpg"] []
                ]
              , div [className "col-md-8"]
                [
                  ul [className "list-group"]
                  [
                    li [className "list-group-item justify-content-between"]
                    [  span [] [text "fonction"]
                    , span [className "badge badge-default badge-pill"] [text "Ensignent checheur"]
                    ]
                  , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Entitte, service"]
                    , span [className "badge badge-default badge-pill"] [text "Mines Saint - Etinene SPIN -PTSI"]
                    ]

                  , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Entitte, service"]
                    , span [className "badge badge-default badge-pill"] [text "Mines Saint - Etinene"]
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
