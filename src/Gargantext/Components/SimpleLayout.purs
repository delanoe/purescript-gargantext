module Gargantext.Components.SimpleLayout where

import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.TopBar (topBar)
import Gargantext.License (license)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.SimpleLayout"

-- Simple layout does not accommodate the tree
type SimpleLayoutProps = (
  handed :: T.Box GT.Handed
  )

simpleLayout :: R2.Component SimpleLayoutProps
simpleLayout = R.createElement simpleLayoutCpt

simpleLayoutCpt :: R.Component SimpleLayoutProps
simpleLayoutCpt = here.component "simpleLayout" cpt
  where
    cpt { handed } children = do
      pure $ H.div { className: "simple-layout" } (
        [ topBar { handed } [] ] <> children <> [ license ]
        )
