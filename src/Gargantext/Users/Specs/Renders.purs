module Gargantext.Users.Specs.Renders
       where

import Gargantext.Users.Types

import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Data.Either (Either(..))
import Data.Generic (gShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (($), (<<<))
import React (ReactElement)
import React.DOM (button, div, h4, li, span, text, ul)
import React.DOM.Props (_id, className)
import React.DOM.Props as RP
import Thermite (Render)


infoRender :: forall props. Tuple String String -> Array ReactElement
infoRender (Tuple title content) =
  [
    span [className "font-weight-bold text-primary"] [text title],
    span [className "pull-right"] [text content]
  ]

listElement :: forall props. Array ReactElement -> ReactElement
listElement = li [className "list-group-item justify-content-between"]

card :: forall props. String -> Array ReactElement -> Array ReactElement
card title elems =
  [
    div [className "card"]
    [
      div [className "card-header text-white bg-primary"]
      [
        h4 [] [text title]
      ],
      div [className "card-body"]
      elems
    ]
  ]

userInfos :: ReactElement
userInfos =
    ul [className "list-group"]
    [
      listElement <<< infoRender $ Tuple "Fonction: " "Enseignant chercheur"
    , listElement <<< infoRender $ Tuple "Entité, service: " "Mines Saint-Etienne SPIN -PTSI"
    , listElement <<< infoRender $ Tuple "Téléphone: " "(+33) 4 77 42 00 70"
    , listElement <<< infoRender $ Tuple "Courriel: " "gargantua@rabelais.fr"
    , listElement <<< infoRender $ Tuple "Bureau: " "D1/10"
    , listElement <<< infoRender $ Tuple "Appelation: " "Maître de conférences (EPA)"
    , listElement <<< infoRender $ Tuple "Lieu: " "Saint-Etienne, 158 Cours Fauriel"
    ]

pbInfos :: ReactElement
pbInfos =
  ul [className "list-group"]
  [
    listElement <<< infoRender $ Tuple "" "https://www.imt.fr/en/"
  , listElement <<< infoRender $ Tuple "" "https://www.imt.fr/en/"
  , listElement <<< infoRender $ Tuple "" "https://www.imt.fr/en/"
  ]

render :: forall props. Render State props Action
render dispatch _ state _ =
  [
    div [className "container-fluid"]
    [
      div [className "row", _id "user-page-info"]
      [
        div [className "row"]
        [
          button [RP.onClick \_ -> dispatch $ FetchUser 452145] [ text "Fetch User"],
          div [className "col-md-8"]
          $ card (case state.user of (Just _) -> "Ok"
                                     Nothing -> "Pas Ok")
          [userInfos]
        ]
      ]
    ]
  ]
