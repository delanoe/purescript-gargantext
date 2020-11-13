module Gargantext.Components.Nodes.Texts.Types where

import Gargantext.Prelude

data SidePanelState = InitialClosed | Opened | Closed
derive instance eqSidePanelState :: Eq SidePanelState

toggleSidePanelState :: SidePanelState -> SidePanelState
toggleSidePanelState InitialClosed = Opened
toggleSidePanelState Closed        = Opened
toggleSidePanelState Opened        = Closed
