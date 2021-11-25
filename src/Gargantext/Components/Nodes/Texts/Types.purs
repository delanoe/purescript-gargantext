module Gargantext.Components.Nodes.Texts.Types where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Types (ListId, NodeID)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Toestand as T

data SidePanelState = InitialClosed | Opened | Closed
derive instance Eq SidePanelState

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


-----------------------------------------------------------------

-- @XXX: This custom context solves a wrong monolithic front design where
--       "DocsTable" component is used for many different use cases
--       Normally we would have use the classic "Gargantext.Components.Reload",
--       but we limit side-effects by using another context reference
textsReloadContext :: R.Context (Maybe (T.Box T2.Reload))
textsReloadContext = R.createContext Nothing
