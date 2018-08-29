module Gargantext.Pages.Corpus.Doc.Annotation where

import Prelude hiding (div)
import React (ReactElement)
import React.DOM (a, button, div, h4, h6, input, li, nav, option, p, select, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, name, onChange, onInput, placeholder, role, style, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)


type State =
  {
    inputValue :: String
  }

initialState :: State
initialState =
  {
    inputValue : ""
  }


data Action
  = NoOp
  | ChangeString String
  | ChangeAnotherString String
  | SetInput String


performAction :: forall props. PerformAction State props Action
performAction NoOp _ _ = pure unit

performAction (ChangeString ps) _ _ = pure unit

performAction (ChangeAnotherString ps) _ _ = pure unit

performAction (SetInput ps) _ _ = void do
  modifyState \( state) ->  state { inputValue = ps }



docview :: forall props. Spec State props Action
docview = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [
          div [className "container1"]
          [
            div [className "row"]
            [
              div [className "col-md-4", style {border : "1px solid black", padding : "34px"}]
              [
                div [className "row"]
                [
                  div [className "col-md-12 input-group mb-3"] [select [className "form-control custom-select",onChange (\e -> dispatch (ChangeString $ (unsafeCoerce e).target.value)) ] $ map optps aryPS ]
                , div [className "col-md-12 form-control input-group mb-3"] [ select [className "form-control custom-select",onChange (\e -> dispatch (ChangeAnotherString $ (unsafeCoerce e).target.value)) ] $ map optps aryPS1 ]
                ]
              , div [className "row", style { marginTop : "35px"}]
                [
                  nav [ ]
                  [ div [className "nav nav-tabs", _id "nav-tab",role "tablist"]
                    [ a [className "nav-item nav-link active",_id "nav-home-tab"   ,  _data {toggle : "tab"},href "#nav-home"   ,role "tab",aria {controls : "nav-home"}   ,aria {selected:true}] [ text "STOPLIST"]
                    , a [className "nav-item nav-link"       ,_id "nav-profile-tab",  _data {toggle : "tab"},href "#nav-profile",role "tab",aria {controls : "nav-profile"},aria {selected:true}] [ text "MAINLIST"]
                    , a [className "nav-item nav-link"       ,_id "nav-contact-tab",  _data {toggle : "tab"},href "#nav-contact",role "tab",aria {controls : "nav-contact"},aria {selected:true}] [ text "MAPLIST"]

                    ]
                  ]
                , div [className "tab-content" , _id "nav-tabContent"]
                  [
                    div [ className "tab-pane fade show active"
                        , role "tabpanel"
                        , aria {labelledby : "nav-home-tab"}
                        , _id "nav-home"
                        ]
                    [
                      h6 [] [text "Add a free term to STOPLIST"]
                    ,  div [className "form-group"]
                       [ input [className "form-control", _id "id_password", name "password", placeholder "Any text", _type "value",value state.inputValue,onInput \e -> dispatch (SetInput (unsafeEventValue e))]
                       , div [className "clearfix"] []
                       ]
                    , button [className "btn btn-primary", _type "button"] [text "Create and Add"]
                    ]

                  , div [ className "tab-pane fade show"
                        , role "tabpanel"
                        , aria {labelledby : "nav-profile-tab"}
                        , _id "nav-profile"
                        ]
                    [ ]
                  , div [ className "tab-pane fade show"
                        , role "tabpanel"
                        , aria {labelledby : "nav-contact-tab"}
                        , _id "nav-contact"
                        ]
                    [ ]
                  ]
                ]
              ]
            , div [className "col-md-8"]
              [ h4 [] [text "Ultrasonic sensors in urban traffic driving-aid systems"]
              , ul [className "list-group"]
                [ li [className "list-group-item justify-content-between"]
                  [  span [] [text "Sensors (Basel, switzerland)"]
                  , span [className "badge badge-default badge-pill"] [text "source"]
                  ]
                , li [className "list-group-item justify-content-between"]
                  [  a [href "http://localhost:2015/#/userPage"] [text "Luciano Alonso, Vicente Milanes, Carlos Torre-Ferarro, Jorge Godoy, Juan P oria, Teresa de pedro"]
                  , span [className "badge badge-default badge-pill"] [text "authors"]
                  ]

                , li [className "list-group-item justify-content-between"]
                  [  span [] [text "2011-01-11 0.00"]
                  , span [className "badge badge-default badge-pill"] [text "date"]
                  ]
                ]
              , span [className "badge badge-default badge-pill"] [text "abstract"]
              , p [] [text "It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like)."]
              , div [className "jumbotron"]
                [ p [] [text "Empty Full Text"]
                ]
              ]
            ]
          ]
      ]

aryPS :: Array String
aryPS = ["STOPLIST", "MAINLIST", "MAPLIST"]

aryPS1 :: Array String
aryPS1 = ["Nothing Selected","STOPLIST", "MAINLIST", "MAPLIST"]


optps :: String -> ReactElement
optps val = option [ value  val ] [text  val]

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
