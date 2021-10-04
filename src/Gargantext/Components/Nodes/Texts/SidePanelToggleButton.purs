module Gargantext.Components.Nodes.Texts.SidePanelToggleButton
  ( Props, sidePanelToggleButton
  ) where

import Prelude
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Nodes.Texts.Types
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Texts.SidePanelToggleButton"

type Props = ( state :: T.Box SidePanelState )

sidePanelToggleButton :: R2.Component Props
sidePanelToggleButton = R.createElement sidePanelToggleButtonCpt

sidePanelToggleButtonCpt :: R.Component Props
sidePanelToggleButtonCpt = here.component "sidePanelToggleButton" cpt
  where
    cpt { state } _ = do
      open' <- T.useLive T.unequal state

      pure $
        H.button { className: "btn btn-primary"
                 , on: { click: \_ -> T.modify_ toggleSidePanelState state } } [ H.text (text open') ]
    text InitialClosed = "Show Side Panel"
    text Opened        = "Hide Side Panel"
    text Closed        = "Show Side Panel"
