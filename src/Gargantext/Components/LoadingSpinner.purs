module Gargantext.Components.LoadingSpinner where

import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.LoadingSpinner"

type Props = ()

loadingSpinner :: Record Props -> R.Element
loadingSpinner props = R.createElement loadingSpinnerCpt props []

loadingSpinnerCpt :: R.Component Props
loadingSpinnerCpt = here.component "LoadingSpinner" cpt
  where
    -- cpt _ _ = H.i {className: "spinner fa fa-smile-o fa-spin fa-3x fa-fw"} [H.text ""]
    -- cpt _ _ = H.i {className: "fa fa-globe fa-spin fa-3x fa-fw"} [H.text ""]

    -- cpt _ _ = H.i {className: "fa fa-circle-o-notch fa-spin fa-3x fa-fw"} [H.text ""]
    cpt _ _ = do
      pure $ H.i {className: "fa fa-spinner fa-pulse fa-3x fa-fw"} [H.text ""]
