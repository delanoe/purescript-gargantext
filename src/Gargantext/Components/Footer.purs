module Gargantext.Components.Footer where

import Gargantext.Prelude
import Gargantext.Sessions as Sessions
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

thisModule :: String
thisModule = "Gargantext.Components.Footer"

---------------------------------------------------------------------------
type FooterProps s = ( session :: s )

footer :: forall cell c. T.Read cell c => Record (FooterProps cell) -> R.Element
footer props = R.createElement footerCpt props []

footerCpt :: forall cell c. T.Read cell c => R.Component (FooterProps cell)
footerCpt = R.hooksComponentWithModule thisModule "footer" cpt where
  cpt { session } _ =
    pure $ H.div { className: "container" } [ H.hr {}, H.footer {} [] ]
