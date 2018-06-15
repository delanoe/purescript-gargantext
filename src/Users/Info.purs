module Users.Info
       (layoutUser
       )
       where

import Prelude hiding (div)
import Users.Types.Lens
import Users.Types.Types

import Brevets as B
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import DocView as DV
import Network.HTTP.Affjax (AJAX)
import Projects as PS
import React.DOM (a, div, h3, h5, h6, i, img, li, nav, small, span, table, tbody, td, text, th, thead, tr, ul)
import React.DOM.Props (_data, _id, aria, className, href, role, scope, src)
import Tab (tabs)
import Thermite (PerformAction, Render, Spec, focus, modifyState, simpleSpec)


layoutUser :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
layoutUser = simpleSpec performAction render
  where
    render :: Render State props Action
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
