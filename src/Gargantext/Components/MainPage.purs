module Gargantext.Components.MainPage where

import Gargantext.Prelude

import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import Web.HTML.Event.EventTypes (offline)

import Gargantext.Components.App.Data (Boxes)
import Gargantext.Routes as GR
import Gargantext.Types as GT
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
    cpt { boxes: { handed
                 , route } } children = do
      handed' <- T.useLive T.unequal handed
      route' <- T.useLive T.unequal route

      let classNameOffsetPre /\ className /\ classNameOffsetPost = case route' of
            GR.PGraphExplorer _ _ -> "" /\ "col-md-12" /\ ""
            _                     -> case handed' of
              GT.LeftHanded  -> "" /\ "col-md-10" /\ "col-md-2"
              GT.RightHanded -> "col-md-2" /\ "col-md-10" /\ ""

      pure $ H.div { className: "row" }
        [ H.div { className: classNameOffsetPre } []
        , H.div { className }
          [ H.div { id: "page-wrapper" }
            [ H.div { className: "container-fluid" } children ]
          ]
        , H.div { className: classNameOffsetPost } []
        ]
