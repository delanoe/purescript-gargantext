module Gargantext.Components.GraphExplorer.ControlsToggleButton
  ( Props, controlsToggleButton, controlsToggleButtonCpt
  ) where

import Prelude (bind, pure, ($))
import Reactix as R
import Reactix.DOM.HTML as H

type Props = ( state :: R.State Boolean )

controlsToggleButton :: Record Props -> R.Element
controlsToggleButton props = R.createElement controlsToggleButtonCpt props []
  
controlsToggleButtonCpt :: R.Component Props
controlsToggleButtonCpt = R.hooksComponent "GraphControlsToggleButton" cpt
  where
    cpt {state} _ = do
      let (open /\ setOpen) = state
      pure $
        RH.button
          { className: "btn btn-primary", on: {click: \_ -> setOpen not } }
          [ RH.text (text open) ]
    text true = "Hide Controls"
    text false = "Show Controls"
