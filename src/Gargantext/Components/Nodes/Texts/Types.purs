module Gargantext.Components.Nodes.Texts.Types where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Types (NodeID)

data SidePanelState = InitialClosed | Opened | Closed
derive instance eqSidePanelState :: Eq SidePanelState

toggleSidePanelState :: SidePanelState -> SidePanelState
toggleSidePanelState InitialClosed = Opened
toggleSidePanelState Closed        = Opened
toggleSidePanelState Opened        = Closed


type SidePanelTriggers = (
    triggerAnnotatedDocIdChange :: R.Ref (Maybe (NodeID -> Effect Unit))
)

emptySidePanelTriggers :: R.Hooks (Record SidePanelTriggers)
emptySidePanelTriggers = do
  triggerAnnotatedDocIdChange <- R.useRef Nothing

  pure $ {
    triggerAnnotatedDocIdChange
    }
