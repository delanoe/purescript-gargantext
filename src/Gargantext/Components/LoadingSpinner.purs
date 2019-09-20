module Gargantext.Components.LoadingSpinner where

import Reactix as R
import Reactix.DOM.HTML as H

type Props = ()

loadingSpinner :: Record Props -> R.Element
loadingSpinner props = R.createElement loadingSpinnerCpt props []

loadingSpinnerCpt :: R.Component Props
loadingSpinnerCpt = R.staticComponent "LoadingSpinner" cpt
  where
    cpt _ _ = H.span {} [H.text "[Loading]"]
