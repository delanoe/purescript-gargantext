module UserPage where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, div, h3, h5, h6, i, img, li, nav, small, span, table, tbody, td, text, th, thead, tr, ul)
import React.DOM.Props (_data, _id, aria, className, href, role, scope, src)
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
            [ span [] [text ""]
            ]
          ]
        , div [className "row", _id "user-page-info"]
          [
            div [className "col-md-12"]
            [ div [className "row"]
              [ div [className "col-md-2"]
                [ img [src "/images/Gargantextuel-212x300.jpg"] []
                ]
              , div [className "col-md-1"] []
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
                    [  span [] [text "Telephone"]
                    , span [className "badge badge-default badge-pill"] [text "04 77 42 0070"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Telephone"]
                    , span [className "badge badge-default badge-pill"] [text "04 77 42 0070"]
                    ]

                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "courriel"]
                    , span [className "badge badge-default badge-pill"] [text "veronica@mines-stsi.fr"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Bureau"]
                    , span [className "badge badge-default badge-pill"] [text "D1/10"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Apellation"]
                    , span [className "badge badge-default badge-pill"] [text "Maitre de reherche (EPA)"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Lieu"]
                    , span [className "badge badge-default badge-pill"] [text "Saint -Etienne, 158 Cours Fauriel"]
                    ]

                  ]
                ]
              ]
            ]
          ]
        , div [className "row",_id "user-page-footer"]
          [ div [className "col-md-12"]
            [ nav []
              [ div [className "nav nav-tabs", _id "nav-tab",role "tablist"]
                [
                  a [className "nav-item nav-link active",_id "nav-home-tab",  _data {toggle : "tab"},href "#nav-home",role "tab",aria {controls : "nav-home"},aria {selected:true}] [ text "Publications (12)"]

                , a [className "nav-item nav-link",_id "nav-profile-tab",  _data {toggle : "tab"},href "#nav-profile",role "tab",aria {controls : "nav-profile"},aria {selected:true}] [ text "Brevets (2)"]

                ,a [className "nav-item nav-link",_id "nav-contact-tab",  _data {toggle : "tab"},href "#nav-contact",role "tab",aria {controls : "nav-contact"},aria {selected:true}] [ text "Projets IMT (5)"]

                ]
              ]
            , div [className "tab-content" , _id "nav-tabContent"]
              [
                div [className "tab-pane fade show active", role "tabpanel", aria {labelledby : "nav-home-tab"}, _id "nav-home"]
                [
                  table [ className "table"]
                  [ thead [ className "thead-dark"]
                    [ tr []
                      [
                        th [ scope "col"]
                        [ text "Date"
                        ]
                      , th [scope "col"]
                        [ text "Description"
                        ]
                      , th [ scope "col"]
                        [ text "Projects"]
                      , th [ scope "col"]
                        [ text " Favorite"]

                      , th [scope "col"]
                        [text "Delete"]

                      ]
                    ]
                  , tbody []
                    [ tr []
                      [ td [] [ text "2012/03/06"]
                      , td [] [ text "USE OF ACOUSTIC TO DISCRIMINATE DAMAGE MODES IN COMPOSITE -ANTENNA - STRUCTURE DURING BUCKLING LOADING "]
                      , td [] [ text "ICEM15: 15TH INTERNATIONAL CONFERENCE ON EXPERIMENTAL MECHANICS"]
                      , td [] [ i [className "fas fa-star"] []]
                      , td [] [ text "delete"]
                      ]
                    ]
                  ]
                ]
              , div [className "tab-pane fade show", role "tabpanel", aria {labelledby : "nav-profile-tab"}, _id "nav-profile"]
                [
                  span [] [text "Lorizzle ipsum bling bling sit amizzle, consectetuer adipiscing elit. Nizzle sapien velizzle, bling bling volutpat, suscipit , gravida vel, arcu. Check it out hizzle that's the shizzle. We gonna chung erizzle. Fo izzle dolor fo turpis tempizzle tempor. Gangsta boom shackalack mofo et turpizzle. Sizzle izzle tortor. Pellentesque uhuh ... yih!"]
                ]
              , div [className "tab-pane fade show", role "tabpanel", aria {labelledby : "nav-contact-tab"}, _id "nav-contact"]
                [
                  span [] [text "Lorizzle ipsum bling bling sit amizzle, consectetuer adipiscing elit. Nizzle sapien velizzle, bling bling volutpat, suscipit , gravida vel, arcu. Check it out hizzle that's the shizzle. We gonna chung erizzle. Fo izzle dolor fo turpis tempizzle tempor. Gangsta boom shackalack mofo et turpizzle. Sizzle izzle tortor. Pellentesque uhuh ... yih!"]
                ]
              ]
            ]
          ]
        ]
      ]
