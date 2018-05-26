module NgramsTable where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import React.DOM (input, table, tbody, td, text, th, thead, tr)
import React.DOM.Props (_type, checked, className, onChange, scope, title)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

type State =
  {
    completed :: Boolean
  }

initialState :: State
initialState =
  {
    completed : true
  }

data Action
  = NoOp
  | ChangeCompleted Boolean


performAction :: forall eff props. PerformAction ( console :: CONSOLE , ajax    :: AJAX, dom     :: DOM | eff ) State props Action
performAction NoOp _ _ = void do
  modifyState id

performAction (ChangeCompleted b)   _ _ = void $ modifyState $ _ { completed = b }


ngramsTableSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
ngramsTableSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [table [ className "table able table-bordered"]
                  [ thead [ className "tableHeader table-bordered"]
                    [ tr []
                      [ th [ scope "col"] [ text "Map" ]
                      , th [ scope "col"] [ text "Stop"]
                      , th [ scope "col"] [ text "Terms"]
                      , th [ scope "col"] [ text "Occurences(nb)" ]
                      ]
                    ]
                  , tbody []

                    [ tr [] [
                         td [] [ input_checkbox]
                            , td [][ input_checkbox]
                            , td [] [ text "India"]
                            , td [] [ text "807"]
                            ]
                    , tr [] [ td [][  input_checkbox]
                            , td [] [ input_checkbox]

                            , td [] [ text "Tobacco use"]
                            , td [] [ text "351"]
                            ]
                    , tr [] [ td []
                              [  input_checkbox]
                            , td []
                              [ input_checkbox]
                            , td [] [ text "tobacco"]
                            , td [] [ text "336"]
                            ]
                    ,tr [] [ td []
                              [  input_checkbox]
                            , td []
                              [ input_checkbox]
                            , td [] [ text "studies"]
                            , td [] [ text "282"]
                            ]
                    , tr [] [ td []
                              [  input_checkbox]
                            , td []
                              [ input_checkbox]
                            , td [] [ text "prevalence"]
                            , td [] [ text "217"]
                            ]

                    , tr [] [ td []
                              [  input_checkbox]
                            , td []
                              [ input_checkbox]
                            , td [] [ text "smoking"]
                            , td [] [ text "169"]
                            ]
                    ]
                  ]
      ]
      where
        input_checkbox = input [ _type "checkbox"
                                    , className "checkbox"
                                    , checked state.completed
                                    , title "Mark as completed"
                                    , onChange \e -> dispatch (ChangeCompleted (unsafeCoerce e).target.checked)
                                    ] []
