module Gargantext.Components.Nodes.Texts.Types where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Types (ListId, NodeID)
import Gargantext.Utils.Reactix as R2

data SidePanelState = InitialClosed | Opened | Closed
derive instance eqSidePanelState :: Eq SidePanelState

toggleSidePanelState :: SidePanelState -> SidePanelState
toggleSidePanelState InitialClosed = Opened
toggleSidePanelState Closed        = Opened
toggleSidePanelState Opened        = Closed

type TriggerAnnotatedDocIdChangeParams = (
    corpusId :: NodeID
  , listId   :: ListId
  , nodeId   :: NodeID
  )

type SidePanelTriggers = (
    triggerAnnotatedDocIdChange :: R2.Trigger (Record TriggerAnnotatedDocIdChangeParams)
  , triggerSidePanel            :: R2.Trigger Unit
)

emptySidePanelTriggers :: R.Hooks (Record SidePanelTriggers)
emptySidePanelTriggers = do
  triggerAnnotatedDocIdChange <- R.useRef Nothing
  triggerSidePanel            <- R.useRef Nothing

  pure $ {
      triggerAnnotatedDocIdChange
    , triggerSidePanel
    }


type TextsLayoutControls = (
    showSidePanel :: R.State SidePanelState
  , triggers      :: Record SidePanelTriggers
  )

initialControls :: R.Hooks (Record TextsLayoutControls)
initialControls = do
  showSidePanel  <- R.useState' Opened
  triggers <- emptySidePanelTriggers

  pure $ {
      showSidePanel
    , triggers
  }
