module Gargantext.Components.Nodes.Texts.SidePanelToggleButton
  ( Props, sidePanelToggleButton
  ) where

import Data.Tuple.Nested ((/\))
import Prelude
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Nodes.Texts.Types
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Texts.SidePanelToggleButton"

type Props = ( state :: R.State SidePanelState )

sidePanelToggleButton :: R2.Component Props
sidePanelToggleButton = R.createElement sidePanelToggleButtonCpt

sidePanelToggleButtonCpt :: R.Component Props
sidePanelToggleButtonCpt = R.hooksComponentWithModule thisModule "sidePanelToggleButton" cpt
  where
    cpt { state } _ = do
      let (open /\ setOpen) = state

      pure $
        H.button { className: "btn btn-primary"
                 , on: { click: \_ -> setOpen $ toggleSidePanelState } } [ H.text (text open) ]
    text InitialClosed = "Show Side Panel"
    text Opened        = "Hide Side Panel"
    text Closed        = "Show Side Panel"
