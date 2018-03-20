module AnnotationDocumentView where

import Prelude hiding (div)
import React (ReactElement)
import React.DOM (a, button, div, h4, input, li, nav, option, p, select, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, onChange, role, selected, value)
import Thermite (PerformAction, Render, Spec, cotransform, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)


type State = String

initialState :: State
initialState = ""


data Action
  = NoOp
  | ChangeString String
  | ChangeAnotherString String


performAction :: PerformAction _ State _ Action
performAction NoOp _ _ = pure unit

performAction (ChangeString ps) _ _ = pure unit

performAction (ChangeAnotherString ps) _ _ = pure unit



docview :: Spec _ State _ Action
docview = simpleSpec performAction render
  where
    render :: Render State _ Action
    render dispatch _ state _ =
      [ div [className "container"]
        [ div [className "row"]
          [ div [className "col-md-4"]
            [ div [className "row"]
              [ div [className "col-md-12 form-control"] [select [onChange (\e -> dispatch (ChangeString $ (unsafeCoerce e).target.value)) ] $ map optps aryPS ]
              , div [className "col-md-12 form-control"] [ select [onChange (\e -> dispatch (ChangeAnotherString $ (unsafeCoerce e).target.value)) ] $ map optps aryPS1 ]
              ]
            , div [className "row"]
              [
                nav []
              [ div [className "nav nav-tabs", _id "nav-tab",role "tablist"]
                [ a [className "nav-item nav-link active",_id "nav-home-tab"   ,  _data {toggle : "tab"},href "#nav-home"   ,role "tab",aria {controls : "nav-home"}   ,aria {selected:true}] [ text "Publications (12)"]
                , a [className "nav-item nav-link"       ,_id "nav-profile-tab",  _data {toggle : "tab"},href "#nav-profile",role "tab",aria {controls : "nav-profile"},aria {selected:true}] [ text "Brevets (2)"]
                , a [className "nav-item nav-link"       ,_id "nav-contact-tab",  _data {toggle : "tab"},href "#nav-contact",role "tab",aria {controls : "nav-contact"},aria {selected:true}] [ text "Projets (5)"]

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
                        input [] []
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
                    [  span [] [text "Luciano Alonso, Vicente Milanes, Carlos Torre-Ferarro, Jorge Godoy, Juan P oria, Teresa de pedro"]
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





-- ul [className "nav nav-pills mb-3", _id "pills-tab",role "tablist"]
--                 [ li [className "nav-item"]
--                   [ a [className "nav-link active", _id "pills-stop", _data {toggle : "pill"}, href "#pills-stop", role "tab", aria {controls :"pills-stop"}, aria {selected : true}] []
--                   ]
--                 , li [className "nav-item"]
--                   [ a [className "nav-link", _id "pills-main", _data {toggle : "pill"}, href "#pills-main", role "tab", aria {controls :"pills-main"}, aria {selected : false}] []
--                   ]
--                 , li [className "nav-item"]
--                   [ a [className "nav-link", _id "pills-map", _data {toggle : "pill"}, href "#pills-map", role "tab", aria {controls :"pills-map"}, aria {selected : false}] []
--                   ]
--                 ]
--               , div [className "tab-content", _id "pills-tabContent"]
--                 [ div [className "tab-pane fade show active", _id "pills-stop", role "tabpanel" ,aria {labelledby : "pills-stop-tab"}] []
--                 , div [ className "tab-pane fade", _id "pills-main", role "tabpanel" ,aria {labelledby : "pills-main-tab"}] []
--                 , div [ className "tab-pane fade", _id "pills-map", role "tabpanel" ,aria {labelledby : "pills-map-tab"}] []
--                 ]
