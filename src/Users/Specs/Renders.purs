module Users.Specs.Renders
       where

import Data.Tuple (Tuple(..))
import Prelude (($), (<<<), (<>))
import React (ReactElement)
import React.DOM (div, h1, h3, h4, img, li, span, text, ul)
import React.DOM.Props (_id, className, src)
import Thermite (Render)
import Users.Types.Types (Action, State)


infoRender :: forall props. Tuple String String -> Array ReactElement
infoRender (Tuple title content) =
  [
    span [] [text title],
    span [className ""] [text content]
  ]

listElement :: forall props. Array ReactElement -> ReactElement
listElement = li [className "list-group-item justify-content-between"]

card :: forall props. String -> Array ReactElement -> Array ReactElement
card title elems =
  [
    div [className "card"]
    [
      div [className "card-header"]
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
    ,listElement <<< infoRender $ Tuple "Entité, service: " "Mines Saint-Etienne SPIN -PTSI"
    ,listElement <<< infoRender $ Tuple "Téléphone: " "(+33) 4 77 42 00 70"
    ,listElement <<< infoRender $ Tuple "Courriel: " "gargantua@rabelais.fr"
    ,listElement <<< infoRender $ Tuple "Bureau: " "D1/10"
    ,listElement <<< infoRender $ Tuple "Appelation: " "Maître de conférences (EPA)"
    ,listElement <<< infoRender $ Tuple "Lieu: " "Saint-Etienne, 158 Cours Fauriel"
    ]

pbInfos :: ReactElement
pbInfos =
  ul [className "list-group"]
  [
    listElement <<< infoRender $ Tuple "" "https://www.imt.fr/en/"
  ,listElement <<< infoRender $ Tuple "" "https://www.imt.fr/en/"
  ,listElement <<< infoRender $ Tuple "" "https://www.imt.fr/en/"
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
          div [className "col-md-8"]
          $ card "Jean HEUDE" [userInfos]
        ,div [className "col-md-4"]
         $
         card "Publications" [pbInfos]
         <> card "Brevets" [pbInfos]
        ]
      ]
    ]
  ]
