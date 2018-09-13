module Gargantext.Pages.Corpus.User.Users.Specs.Renders
       where

import Gargantext.Pages.Corpus.User.Users.Types

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (($), (<<<))
import React (ReactElement)
import React.DOM (div, h3, img, li, span, text, ul)
import React.DOM.Props (_id, className, src)
import Thermite (Render)


render :: Render State {} Action
render dispatch _ state _ =
  [
          div [className "col-md-12"]
          $ case state.user of
            (Just (User user)) -> display user.name [userInfos user.hyperdata]
            Nothing -> display "User not found" []
  ]

display :: String -> Array ReactElement -> Array ReactElement
display title elems =
  [ div [className "container-fluid"]
    [ div [className "row", _id "user-page-header"]
          [ div [className "col-md-6"] [ h3 [] [text title] ]
          , div [className "col-md-8"] []
          , div [className "col-md-2"] [ span [] [text ""] ]
          ]
    , div [className "row", _id "user-page-info"]
          [ div [className "col-md-12"]
            [ div [className "row"]
              [ div [className "col-md-2"]
                    [ img [src "/images/Gargantextuel-212x300.jpg"] ]
              , div [className "col-md-1"] []
              , div [className "col-md-8"] elems
              ]
            ]
          ]
     ]
  ]

userInfos :: HyperData -> ReactElement
userInfos (HyperData user) =
    ul [className "list-group"]
    [
      listElement <<< infoRender <<< Tuple "Fonction: "        $ checkMaybe user.fonction
    , listElement <<< infoRender <<< Tuple "Entité, service: " $ checkMaybe user.entite
    , listElement <<< infoRender <<< Tuple "Téléphone: "       $ checkMaybe user.atel
    , listElement <<< infoRender <<< Tuple "Courriel: "        $ checkMaybe user.mail
    , listElement <<< infoRender <<< Tuple "Bureau: "          $ checkMaybe user.bureau
    , listElement <<< infoRender <<< Tuple "Appelation: "      $ checkMaybe user.fonction
    , listElement <<< infoRender $   Tuple "Lieu: "            $ checkMaybe user.lieu
    ]
  where
    checkMaybe (Nothing) = ""
    checkMaybe (Just a) = a

    listElement :: Array ReactElement -> ReactElement
    listElement = li [className "list-group-item justify-content-between"]

    infoRender :: Tuple String String -> Array ReactElement
    infoRender (Tuple title content) =
      [ span [] [text title]
      , span [className "badge badge-default badge-pill"] [text content]
      ]
