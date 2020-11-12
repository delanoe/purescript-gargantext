module Gargantext.Components.Footer where

import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Sessions as Sessions

thisModule :: String
thisModule = "Gargantext.Components.Footer"

---------------------------------------------------------------------------
type FooterProps =
  (
    session :: Sessions.Session
  )

footer :: Record FooterProps -> R.Element
footer props = R.createElement footerCpt props []

footerCpt :: R.Component FooterProps
footerCpt = R.hooksComponentWithModule thisModule "footer" cpt
  where
    cpt { session } _ = do
      pure $ H.div
              { className: "container" }
              [ H.hr {}
              , H.footer {} []
              ]
