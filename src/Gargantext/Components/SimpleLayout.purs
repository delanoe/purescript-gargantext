module Gargantext.Components.SimpleLayout where

import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.TopBar (topBar)
import Gargantext.License (license)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.SimpleLayout"

-- Simple layout does not accommodate the tree
type SimpleLayoutProps = (
  handed :: R.State GT.Handed
  )

simpleLayout :: R2.Component SimpleLayoutProps
simpleLayout = R.createElement simpleLayoutCpt

simpleLayoutCpt :: R.Component SimpleLayoutProps
simpleLayoutCpt = R.hooksComponentWithModule thisModule "simpleLayout" cpt
  where
    cpt { handed } children = do
      pure $ H.div { className: "simple-layout" } (
        [ topBar { handed } [] ] <> children <> [ license ]
        )
