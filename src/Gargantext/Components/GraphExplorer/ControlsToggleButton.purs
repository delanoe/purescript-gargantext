module Gargantext.Components.GraphExplorer.ControlsToggleButton
  ( Props, controlsToggleButton, controlsToggleButtonCpt
  ) where

import Data.Tuple.Nested ((/\))
import Prelude
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Utils.Reactix as R2

here = R2.here "Gargantext.Components.GraphExplorer.ControlsToggleButton"

type Props = ( state :: R.State Boolean )

controlsToggleButton :: Record Props -> R.Element
controlsToggleButton props = R.createElement controlsToggleButtonCpt props []

controlsToggleButtonCpt :: R.Component Props
controlsToggleButtonCpt = here.component "controlsToggleButton" cpt
  where
    cpt {state} _ = do
      let (open /\ setOpen) = state
      pure $
        H.button
          { className: "btn btn-primary", on: {click: \_ -> setOpen not } }
          [ H.text (text open) ]
    text true = "Hide Controls"
    text false = "Show Controls"
