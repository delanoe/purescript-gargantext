module Gargantext.Components.MainPage where

import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.App.Data (Boxes)
import Gargantext.Routes as GR
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.MainPage"

type MainPage =
  (
    boxes    :: Boxes
  )

mainPage :: R2.Component MainPage
mainPage = R.createElement mainPageCpt
mainPageCpt :: R.Component MainPage
mainPageCpt = here.component "mainPage" cpt
  where
    cpt { boxes: { route } } children = do
      route' <- T.useLive T.unequal route

      let classNameOffset /\ className = case route' of
            GR.PGraphExplorer _ _ -> "" /\ "col-md-12"
            _                     -> "col-md-2" /\ "col-md-10"

      pure $ H.div { className: "row" }
        [ H.div { className: classNameOffset } []
        , H.div { className }
          [ H.div { id: "page-wrapper" }
            [ H.div { className: "container-fluid" } children ]
          ]
        ]
