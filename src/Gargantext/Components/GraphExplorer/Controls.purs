module Gargantext.Components.GraphExplorer.Controls
 ( Controls
 , useGraphControls
 , controls
 , controlsCpt
 ) where

import Data.Array as A
import Data.Int as I
import Data.Maybe (Maybe(..), maybe)
import Data.Sequence as Seq
import Data.Set as Set
import Effect (Effect)
import Effect.Timer (setTimeout)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH
import Toestand as T

import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Button (centerButton, cameraButton)
import Gargantext.Components.GraphExplorer.RangeControl (edgeConfluenceControl, edgeWeightControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.SlideButton (labelSizeButton, mouseSelectorSizeButton)
import Gargantext.Components.GraphExplorer.ToggleButton (multiSelectEnabledButton, edgesToggleButton, louvainToggleButton, pauseForceAtlasButton)
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Controls"

type Controls =
  ( edgeConfluence     :: T.Box Range.NumberRange
  , edgeWeight         :: T.Box Range.NumberRange
  , forceAtlasState    :: T.Box SigmaxT.ForceAtlasState
  , graph              :: SigmaxT.SGraph
  , graphId            :: GET.GraphId
  , graphStage         :: T.Box Graph.Stage
  , hyperdataGraph     :: GET.HyperdataGraph
  , multiSelectEnabled :: T.Box Boolean
  , nodeSize           :: T.Box Range.NumberRange
  , reloadForest       :: T2.ReloadS
  , removedNodeIds     :: T.Box SigmaxT.NodeIds
  , selectedNodeIds    :: T.Box SigmaxT.NodeIds
  , session            :: Session
  , showControls       :: T.Box Boolean
  , showEdges          :: T.Box SigmaxT.ShowEdgesState
  , showLouvain        :: T.Box Boolean
  , sidePanelState     :: T.Box GT.SidePanelState
  , showTree           :: T.Box Boolean
  , sigmaRef           :: R.Ref Sigmax.Sigma
  )

type LocalControls = ( labelSize :: T.Box Number, mouseSelectorSize :: T.Box Number )

initialLocalControls :: R.Hooks (Record LocalControls)
initialLocalControls = do
  labelSize <- T.useBox 14.0
  mouseSelectorSize <- T.useBox 15.0
  pure $ { labelSize, mouseSelectorSize }

controls :: R2.Component Controls
controls = R.createElement controlsCpt

controlsCpt :: R.Component Controls
controlsCpt = here.component "controls" cpt
  where
    cpt { edgeConfluence
        , edgeWeight
        , forceAtlasState
        , graph
        , graphId
        , graphStage
        , hyperdataGraph
        , multiSelectEnabled
        , nodeSize
        , reloadForest
        , selectedNodeIds
        , session
        , showControls
        , showEdges
        , showLouvain
        , showTree
        , sidePanelState
        , sigmaRef } _ = do
      forceAtlasState' <- T.useLive T.unequal forceAtlasState
      graphStage' <- T.useLive T.unequal graphStage
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds
      showControls' <- T.useLive T.unequal showControls
      sidePanelState' <- T.useLive T.unequal sidePanelState

      localControls <- initialLocalControls
      -- ref to track automatic FA pausing
      -- If user pauses FA before auto is triggered, clear the timeoutId
      mFAPauseRef <- R.useRef Nothing

      -- When graph is changed, cleanup the mFAPauseRef so that forceAtlas
      -- timeout is retriggered.
      R.useEffect' $ do
        case graphStage' of
          Graph.Init -> R.setRef mFAPauseRef Nothing
          _          -> pure unit

      -- Handle case when FA is paused from outside events, eg. the automatic timer.
      R.useEffect' $ Sigmax.handleForceAtlas2Pause sigmaRef forceAtlasState mFAPauseRef

      -- Handle automatic edge hiding when FA is running (to prevent flickering).
      R.useEffect2' sigmaRef forceAtlasState' $ do
        T.modify_ (SigmaxT.forceAtlasEdgeState forceAtlasState') showEdges

      -- Automatic opening of sidebar when a node is selected (but only first time).
      R.useEffect' $ do
        if sidePanelState' == GT.InitialClosed && (not Set.isEmpty selectedNodeIds') then
          T.write_ GT.Opened sidePanelState
        else
          pure unit

      -- Timer to turn off the initial FA. This is because FA eats up lot of
      -- CPU, has memory leaks etc.
      R.useEffect1' forceAtlasState' $ do
        if forceAtlasState' == SigmaxT.InitialRunning then do
          timeoutId <- setTimeout 9000 $ do
            case forceAtlasState' of
              SigmaxT.InitialRunning ->
                T.write_ SigmaxT.Paused forceAtlasState
              _ -> pure unit
            R.setRef mFAPauseRef Nothing
          R.setRef mFAPauseRef $ Just timeoutId
          pure unit
         else
           pure unit

      let edgesConfluenceSorted = A.sortWith (_.confluence) $ Seq.toUnfoldable $ SigmaxT.graphEdges graph
      let edgeConfluenceMin = maybe 0.0 _.confluence $ A.head edgesConfluenceSorted
      let edgeConfluenceMax = maybe 100.0 _.confluence $ A.last edgesConfluenceSorted
      let edgeConfluenceRange = Range.Closed { min: edgeConfluenceMin, max: edgeConfluenceMax }

      --let edgesWeightSorted = A.sortWith (_.weight) $ Seq.toUnfoldable $ SigmaxT.graphEdges graph
      --let edgeWeightMin = maybe 0.0 _.weight $ A.head edgesWeightSorted
      --let edgeWeightMax = maybe 100.0 _.weight $ A.last edgesWeightSorted
      --let edgeWeightRange = Range.Closed { min: edgeWeightMin, max: edgeWeightMax }
      let edgeWeightRange = Range.Closed {
           min: 0.0
         , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph
         }

      let nodesSorted = A.sortWith (_.size) $ Seq.toUnfoldable $ SigmaxT.graphNodes graph
      let nodeSizeMin = maybe 0.0 _.size $ A.head nodesSorted
      let nodeSizeMax = maybe 100.0 _.size $ A.last nodesSorted
      let nodeSizeRange = Range.Closed { min: nodeSizeMin, max: nodeSizeMax }

      let className = "navbar navbar-expand-lg " <> if showControls' then "" else "d-none"

      pure $ RH.nav { className }
                 [ RH.ul { className: "navbar-nav mx-auto" }
                   [ -- change type button (?)
                     navItem [ centerButton sigmaRef ]
                   , navItem [ pauseForceAtlasButton { state: forceAtlasState } [] ]
                   , navItem [ edgesToggleButton { state: showEdges } [] ]
                   , navItem [ louvainToggleButton { state: showLouvain } [] ]
                   , navItem [ edgeConfluenceControl { range: edgeConfluenceRange
                                                     , state: edgeConfluence } [] ]
                   , navItem [ edgeWeightControl { range: edgeWeightRange
                                                 , state: edgeWeight } [] ]
                   -- change level
                   -- file upload
                   -- run demo
                   -- search button
                   -- search topics
                   , navItem [ labelSizeButton sigmaRef localControls.labelSize ] -- labels size: 1-4
                   , navItem [ nodeSizeControl { range: nodeSizeRange
                                               , state: nodeSize } [] ]
                   -- zoom: 0 -100 - calculate ratio
                   , navItem [ multiSelectEnabledButton { state: multiSelectEnabled } [] ]  -- toggle multi node selection
                   -- save button
                   , navItem [ mouseSelectorSizeButton sigmaRef localControls.mouseSelectorSize ]
                   , navItem [ cameraButton { id: graphId
                                            , hyperdataGraph: hyperdataGraph
                                            , session: session
                                            , sigmaRef: sigmaRef
                                            , reloadForest } ]
                   ]
                 ]
      where
        navItem = RH.li { className: "nav-item" }
          --   RH.ul {} [ -- change type button (?)
          --     RH.li {} [ centerButton sigmaRef ]
          --   , RH.li {} [ pauseForceAtlasButton {state: forceAtlasState} ]
          --   , RH.li {} [ edgesToggleButton {state: showEdges} ]
          --   , RH.li {} [ louvainToggleButton showLouvain ]
          --   , RH.li {} [ edgeConfluenceControl edgeConfluenceRange edgeConfluence ]
          --   , RH.li {} [ edgeWeightControl edgeWeightRange edgeWeight ]
          --     -- change level
          --     -- file upload
          --     -- run demo
          --     -- search button
          --     -- search topics
          --   , RH.li {} [ labelSizeButton sigmaRef localControls.labelSize ] -- labels size: 1-4
          --   , RH.li {} [ nodeSizeControl nodeSizeRange nodeSize ]
          --     -- zoom: 0 -100 - calculate ratio
          --   , RH.li {} [ multiSelectEnabledButton multiSelectEnabled ]  -- toggle multi node selection
          --     -- save button
          --   , RH.li {} [ nodeSearchControl { graph: graph
          --                                  , multiSelectEnabled: multiSelectEnabled
          --                                  , selectedNodeIds: selectedNodeIds } ]
          --   , RH.li {} [ mouseSelectorSizeButton sigmaRef localControls.mouseSelectorSize ]
          --   , RH.li {} [ cameraButton { id: graphId
          --                             , hyperdataGraph: hyperdataGraph
          --                             , session: session
          --                             , sigmaRef: sigmaRef
          --                             , reloadForest: reloadForest } ]
          --   ]
          -- ]

useGraphControls :: { forceAtlasS :: SigmaxT.ForceAtlasState
                    , graph          :: SigmaxT.SGraph
                    , graphId        :: GET.GraphId
                    , hyperdataGraph :: GET.HyperdataGraph
                    , reloadForest   :: T2.ReloadS
                    , session        :: Session
                    , showTree       :: T.Box Boolean
                    , sidePanel      :: T.Box (Maybe (Record GEST.SidePanel))
                    , sidePanelState :: T.Box GT.SidePanelState }
                 -> R.Hooks (Record Controls)
useGraphControls { forceAtlasS
                 , graph
                 , graphId
                 , hyperdataGraph
                 , reloadForest
                 , session
                 , showTree
                 , sidePanel
                 , sidePanelState } = do
  edgeConfluence <- T.useBox $ Range.Closed { min: 0.0, max: 1.0 }
  edgeWeight <- T.useBox $ Range.Closed {
      min: 0.0
    , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph
    }
  forceAtlasState <- T.useBox forceAtlasS
  graphStage      <- T.useBox Graph.Init
  -- multiSelectEnabled <- T.useBox false
  nodeSize <- T.useBox $ Range.Closed { min: 0.0, max: 100.0 }
  -- removedNodeIds <- T.useBox SigmaxT.emptyNodeIds
  -- selectedNodeIds <- T.useBox SigmaxT.emptyNodeIds
  -- showControls    <- T.useBox false
  showEdges <- T.useBox SigmaxT.EShow
  showLouvain <- T.useBox false
  -- sidePanelState   <- T.useBox GT.InitialClosed
  sigma <- Sigmax.initSigma
  sigmaRef <- R.useRef sigma

  { multiSelectEnabled, removedNodeIds, selectedNodeIds, showControls } <- GEST.focusedSidePanel sidePanel

  pure { edgeConfluence
       , edgeWeight
       , forceAtlasState
       , graph
       , graphId
       , graphStage
       , hyperdataGraph
       , multiSelectEnabled
       , nodeSize
       , removedNodeIds
       , selectedNodeIds
       , session
       , showControls
       , showEdges
       , showLouvain
       , sidePanelState
       , showTree
       , sigmaRef
       , reloadForest
       }
