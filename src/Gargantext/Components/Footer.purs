module Gargantext.Components.Footer where

import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Footer"

---------------------------------------------------------------------------
type FooterProps = ( )

footer :: R2.Component FooterProps
footer = R.createElement footerCpt

footerCpt :: R.Component FooterProps
footerCpt = here.component "footer" cpt where
  cpt { } _ = do
    pure $ H.div { className: "container" } [ H.hr {}, H.footer {} [] ]
