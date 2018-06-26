module Gargantext.Users.Specs.Renders
       where

import Gargantext.Users.Types

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

userInfos :: HyperData -> ReactElement
userInfos (HyperData user) =
    ul [className "list-group"]
    [
      listElement <<< infoRender <<< Tuple "Fonction: " $ checkMaybe user.fonction
    , listElement <<< infoRender <<< Tuple "Entité, service: " $ checkMaybe user.entite
    , listElement <<< infoRender <<< Tuple "Téléphone: " $ checkMaybe user.atel
    , listElement <<< infoRender <<< Tuple "Courriel: " $ checkMaybe user.mail
    , listElement <<< infoRender <<< Tuple "Bureau: " $ checkMaybe user.bureau
    , listElement <<< infoRender <<< Tuple "Appelation: " $ checkMaybe user.fonction
    , listElement <<< infoRender $ Tuple "Lieu: " $ checkMaybe user.lieu
    ]
    where checkMaybe (Nothing) = ""
          checkMaybe (Just a) = a

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
          $ case state.user of
            (Just (User user)) -> card user.name [userInfos user.hyperdata]
            Nothing -> card "Aucun utilisateur" []
        ]
      ]
    ]
  ]
