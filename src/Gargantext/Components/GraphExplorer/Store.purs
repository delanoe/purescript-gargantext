module Gargantext.Components.GraphExplorer.Store
  ( Store
  , State
  , options
  , context
  , provide
  , use
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Types as GT
import Gargantext.Utils (getter)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Stores as Stores
import Reactix as R
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Store"

type Store =
  -- Data
  ( graph              :: T.Box SigmaxT.SGraph
  , graphId            :: T.Box GET.GraphId
  , mMetaData          :: T.Box (Maybe GET.MetaData)
  , hyperdataGraph     :: T.Box GET.HyperdataGraph
  -- Layout
  , showControls       :: T.Box Boolean
  , sideTab            :: T.Box GET.SideTab
  , showSidebar        :: T.Box GT.SidePanelState
  , showDoc            :: T.Box (Maybe GET.GraphSideDoc)
  , expandSelection    :: T.Box Boolean
  , expandNeighborhood :: T.Box Boolean
  -- Controls
  , multiSelectEnabled :: T.Box Boolean
  , edgeConfluence     :: T.Box Range.NumberRange
  , edgeWeight         :: T.Box Range.NumberRange
  , forceAtlasState    :: T.Box SigmaxT.ForceAtlasState
  , graphStage         :: T.Box GET.Stage
  , nodeSize           :: T.Box Range.NumberRange
  , showEdges          :: T.Box SigmaxT.ShowEdgesState
  , showLouvain        :: T.Box Boolean
  , labelSize          :: T.Box Number
  , mouseSelectorSize  :: T.Box Number
  , startForceAtlas    :: T.Box Boolean
  -- Terms update
  , removedNodeIds     :: T.Box SigmaxT.NodeIds
  , selectedNodeIds    :: T.Box SigmaxT.NodeIds
  )

type State =
  -- Data
  ( graph              :: SigmaxT.SGraph
  , graphId            :: GET.GraphId
  , mMetaData          :: Maybe GET.MetaData
  , hyperdataGraph     :: GET.HyperdataGraph
  -- Layout
  , showControls       :: Boolean
  , sideTab            :: GET.SideTab
  , showSidebar        :: GT.SidePanelState
  , showDoc            :: Maybe GET.GraphSideDoc
  , expandSelection    :: Boolean
  , expandNeighborhood :: Boolean
  -- Controls
  , multiSelectEnabled :: Boolean
  , edgeConfluence     :: Range.NumberRange
  , edgeWeight         :: Range.NumberRange
  , forceAtlasState    :: SigmaxT.ForceAtlasState
  , graphStage         :: GET.Stage
  , nodeSize           :: Range.NumberRange
  , showEdges          :: SigmaxT.ShowEdgesState
  , showLouvain        :: Boolean
  , labelSize          :: Number
  , mouseSelectorSize  :: Number
  , startForceAtlas    :: Boolean
  -- Terms update
  , removedNodeIds     :: SigmaxT.NodeIds
  , selectedNodeIds    :: SigmaxT.NodeIds
  )

options ::
  -- Layout
  { showControls        :: Boolean
  , showDoc             :: Maybe GET.GraphSideDoc
  , showSidebar         :: GT.SidePanelState
  , sideTab             :: GET.SideTab
  , expandSelection     :: Boolean
  , expandNeighborhood  :: Boolean
  -- Controls
  , labelSize           :: Number
  , mouseSelectorSize   :: Number
  , multiSelectEnabled  :: Boolean
  , edgeConfluence      :: Range.NumberRange
  , graphStage          :: GET.Stage
  , nodeSize            :: Range.NumberRange
  , showLouvain         :: Boolean
  , showEdges           :: SigmaxT.ShowEdgesState
  -- Terms update
  , removedNodeIds      :: SigmaxT.NodeIds
  , selectedNodeIds     :: SigmaxT.NodeIds
  }
options =
  -- Layout
  { showControls        : false
  , sideTab             : GET.SideTabLegend
  , showSidebar         : GT.InitialClosed
  , showDoc             : Nothing
  , expandSelection     : getter _.expandSelection GET.defaultCacheParams
  , expandNeighborhood  : getter _.expandNeighborhood GET.defaultCacheParams
  -- Controls
  , multiSelectEnabled  : false
  , labelSize           : 14.0
  , mouseSelectorSize   : 15.0
  , edgeConfluence      : Range.Closed { min: 0.0, max: 1.0 }
  , graphStage          : GET.Init
  , nodeSize            : Range.Closed { min: 0.0, max: 100.0 }
  , showLouvain         : false
  , showEdges           : SigmaxT.EShow
  -- Terms update
  , removedNodeIds      : Set.empty
  , selectedNodeIds     : Set.empty
  }

context :: R.Context (Record Store)
context = R.createContext $ unsafeCoerce unit

provide :: Record State -> Array R.Element -> R.Element
provide values = Stores.provideStore here.name values context

use :: R.Hooks (Record Store)
use = Stores.useStore context
