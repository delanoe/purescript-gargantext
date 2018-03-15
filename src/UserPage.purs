module UserPage where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, div, h3, h5, h6, img, li, nav, small, span, table, tbody, td, text, th, thead, tr, ul)
import React.DOM.Props (_data, _id, aria, className, href, role, scope, src)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)

import DocView as DV

type State = String


initialState :: State
initialState = ""

data Action = NoOp


performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff
                                                 ) State props Action
performAction NoOp _ _ = void do
  modifyState id


userPageSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
userPageSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ -- TODO: div [className "tree"] [DV.toHtml dispatch d.tree]
        div [className "container-fluid"]
        [ div [className "row", _id "user-page-header"]
          [ div [className "col-md-2"]
            [ h3 [] [text "User Name"]
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
                    [  span [] [text "Fonction"]
                    , span [className "badge badge-default badge-pill"] [text "Enseignant chercheur"]
                    ]
                  , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Entité, service"]
                    , span [className "badge badge-default badge-pill"] [text "Mines Saint-Etienne SPIN -PTSI"]
                    ]

                  , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Téléphone"]
                    , span [className "badge badge-default badge-pill"] [text "(+33) 04 77 42 0070"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Courriel"]
                    , span [className "badge badge-default badge-pill"] [text "gargantua@rabelais.fr"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Bureau"]
                    , span [className "badge badge-default badge-pill"] [text "D1/10"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Apellation"]
                    , span [className "badge badge-default badge-pill"] [text "Maître de conférences (EPA)"]
                    ]
                   , li [className "list-group-item justify-content-between"]
                    [  span [] [text "Lieu"]
                    , span [className "badge badge-default badge-pill"] [text "Saint-Etienne, 158 Cours Fauriel"]
                    ]

                  ]
                ]
              ]
            ]
          ]
        , div [className "row",_id "user-page-footer"]
          [ div [className "col-md-12"]
            facets          
            ]
        ]
      ]


facets = [ nav []
              [ div [className "nav nav-tabs", _id "nav-tab",role "tablist"]
                [ a [className "nav-item nav-link active",_id "nav-home-tab"   ,  _data {toggle : "tab"},href "#nav-home"   ,role "tab",aria {controls : "nav-home"}   ,aria {selected:true}] [ text "Publications (12)"]
                , a [className "nav-item nav-link"       ,_id "nav-profile-tab",  _data {toggle : "tab"},href "#nav-profile",role "tab",aria {controls : "nav-profile"},aria {selected:true}] [ text "Brevets (2)"]
                , a [className "nav-item nav-link"       ,_id "nav-contact-tab",  _data {toggle : "tab"},href "#nav-contact",role "tab",aria {controls : "nav-contact"},aria {selected:true}] [ text "Projets (5)"]
                , a [className "nav-item nav-link"       ,_id "nav-contact-tab",  _data {toggle : "tab"},href "#nav-contact",role "tab",aria {controls : "nav-contact"},aria {selected:true}] [ text "All (19)"]
                ]
              ]
            , div [className "tab-content" , _id "nav-tabContent"]
              [
                div [ className "tab-pane fade show active"
                    , role "tabpanel"
                    , aria {labelledby : "nav-home-tab"}
                    , _id "nav-home"
                    ]
                      [ facetExample ]
              
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

facetExample = table [ className "table"]
                  [ thead [ className "thead-dark"]
                    [ tr []
                      [ th [ scope "col"] [ text "Date"        ]
                      , th [ scope "col"] [ text "Description" ]
                      , th [ scope "col"] [ text "Projects"    ]
                      , th [ scope "col"] [ text "Favorite"    ]
                      , th [ scope "col"] [ text "Delete"      ]
                      ]
                    ]
                  , tbody []
                    [ tr [] [ td [] [ text "2012/03/06"]
                            , td [] [ text "Big data and text mining"]
                            , td [] [ text "European funds"]
                            , td [] [ text "True"]
                            , td [] [ text "False"]
                            ]
                    , tr [] [ td [] [ text "2013/03/06"]
                            , td [] [ text "Cryptography"]
                            , td [] [ text "French funds"]
                            , td [] [ text "True"]
                            , td [] [ text "False"]
                            ]
                    , tr [] [ td [] [ text "2013/03/06"]
                            , td [] [ text "Artificial Intelligence"]
                            , td [] [ text "Not found"]
                            , td [] [ text "True"]
                            , td [] [ text "False"]
                            ]
                    ]
                  ]


