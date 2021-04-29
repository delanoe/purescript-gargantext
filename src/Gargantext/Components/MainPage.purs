module Gargantext.Components.MainPage where

import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.MainPage"

type MainPage = ()
  -- ( mClassName :: Maybe String )

mainPage :: R2.Component MainPage
mainPage = R.createElement mainPageCpt
mainPageCpt :: R.Component MainPage
mainPageCpt = here.component "mainPage" cpt
  where
    cpt { } children = do
      pure $ H.div {}
        [ H.div { id: "page-wrapper" }
          [ H.div { className: "container-fluid" } children ]
        ]
