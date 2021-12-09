module Gargantext.Components.GraphExplorer.ControlsToggleButton
  ( Props, controlsToggleButton, controlsToggleButtonCpt
  ) where

import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.ControlsToggleButton"

type Props = ( state :: T.Box Boolean )

controlsToggleButton :: R2.Leaf Props
controlsToggleButton = R2.leafComponent controlsToggleButtonCpt

controlsToggleButtonCpt :: R.Component Props
controlsToggleButtonCpt = here.component "controlsToggleButton" cpt
  where
    cpt { state } _ = do
      open' <- T.useLive T.unequal state
      pure $
        H.button
          { className: "btn btn-primary", on: {click: \_ -> T.modify_ not state } }
          [ H.text (text open') ]
    text true = "Hide Controls"
    text false = "Show Controls"
