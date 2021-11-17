module Gargantext.Components.Footer where

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H


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
