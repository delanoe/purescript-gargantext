module Gargantext.Components.Nodes.Texts.Types where

import Data.Maybe (Maybe(..))
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
    currentDocIdRef             :: R.Ref (Maybe Int)
  , toggleSidePanel             :: R2.Trigger Unit  -- toggles side panel
  , triggerAnnotatedDocIdChange :: R2.Trigger (Record TriggerAnnotatedDocIdChangeParams)
  , triggerSidePanel            :: R2.Trigger Unit  -- opens side panel
)

emptySidePanelTriggers :: R.Hooks (Record SidePanelTriggers)
emptySidePanelTriggers = do
  currentDocIdRef             <- R.useRef Nothing
  toggleSidePanel             <- R.useRef Nothing
  triggerAnnotatedDocIdChange <- R.useRef Nothing
  triggerSidePanel            <- R.useRef Nothing

  pure $ {
      currentDocIdRef
    , toggleSidePanel
    , triggerAnnotatedDocIdChange
    , triggerSidePanel
    }


type TextsLayoutControls = (
    triggers      :: Record SidePanelTriggers
  )

initialControls :: R.Hooks (Record TextsLayoutControls)
initialControls = do
  triggers <- emptySidePanelTriggers

  pure $ {
      triggers
  }


type SidePanel =
  (
    corpusId      :: NodeID
  , listId        :: ListId
  , mCurrentDocId :: Maybe Int
  , nodeId        :: NodeID
  )

initialSidePanel :: Maybe (Record SidePanel)
initialSidePanel = Nothing
