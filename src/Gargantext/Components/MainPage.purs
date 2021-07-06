module Gargantext.Components.MainPage where

import Gargantext.Prelude

import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.App.Data (Boxes)
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

      pure $

        H.div { id: "page-wrapper" }
        [
          H.div { className: "container-fluid" } children
        ]
