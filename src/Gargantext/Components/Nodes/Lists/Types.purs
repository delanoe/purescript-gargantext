module Gargantext.Components.Nodes.Lists.Types where

import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson, (~>), (:=))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Reactix as R

import Gargantext.Prelude
import Gargantext.Types (ListId, NodeID)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists.Types"

data CacheState = CacheOn | CacheOff

derive instance genericCacheState :: Generic CacheState _
instance eqCacheState :: Eq CacheState where
  eq = genericEq
instance decodeJsonCacheState :: DecodeJson CacheState where
  decodeJson json = do
    obj <- decodeJson json
    case obj of
      "CacheOn"  -> pure CacheOn
      "CacheOff" -> pure CacheOff
      s          -> Left $ AtKey s $ TypeMismatch $ "Unknown cache value"
instance encodeJsonCacheState :: EncodeJson CacheState where
  encodeJson CacheOn  = encodeJson "CacheOn"
  encodeJson CacheOff = encodeJson "CacheOff"
instance showCacheState :: Show CacheState where
  show = genericShow


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
    toggleSidePanel             :: R2.Trigger Unit  -- toggles side panel
  , triggerSidePanel            :: R2.Trigger Unit  -- opens side panel
)

emptySidePanelTriggers :: R.Hooks (Record SidePanelTriggers)
emptySidePanelTriggers = do
  toggleSidePanel             <- R.useRef Nothing
  triggerSidePanel            <- R.useRef Nothing

  pure $ {
      toggleSidePanel
    , triggerSidePanel
    }


type ListsLayoutControls = (
    triggers      :: Record SidePanelTriggers
  )

initialControls :: R.Hooks (Record ListsLayoutControls)
initialControls = do
  triggers <- emptySidePanelTriggers

  pure $ {
      triggers
  }
