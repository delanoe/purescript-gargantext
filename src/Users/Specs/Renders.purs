module Users.Specs.Renders
       where

import React.DOM (div, h3, img, li, span, text, ul)
import React.DOM.Props (_id, className, src)
import Thermite (Render)
import Users.Types.Types (Action, State)

render :: forall props. Render State props Action
render dispatch _ state _ =
  [
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
        []

      ]
    ]
  ]
