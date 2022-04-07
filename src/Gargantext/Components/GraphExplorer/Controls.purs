module Gargantext.Components.GraphExplorer.Controls
 ( Controls
 , useGraphControls
 , controls
 , controlsCpt
 ) where

import Prelude

import Data.Array as A
import Data.Foldable (intercalate)
import Data.Int as I
import Data.Maybe (Maybe(..), maybe)
import Data.Sequence as Seq
import Data.Set as Set
import Effect.Timer (setTimeout)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.Resources as Graph
import Gargantext.Components.GraphExplorer.Buttons (centerButton, cameraButton, edgesToggleButton, louvainToggleButton, pauseForceAtlasButton, multiSelectEnabledButton)
import Gargantext.Components.GraphExplorer.RangeControl (edgeConfluenceControl, edgeWeightControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.GraphExplorer.SlideButton (labelSizeButton, mouseSelectorSizeButton)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

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
  , sideTab            :: T.Box GET.SideTab
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
        , showEdges
        , showLouvain
        , sidePanelState
        , sideTab
        , sigmaRef } _ = do

--    | States
--    |

      forceAtlasState' <- T.useLive T.unequal forceAtlasState
      graphStage' <- T.useLive T.unequal graphStage
      selectedNodeIds' <- T.useLive T.unequal selectedNodeIds
      sidePanelState' <- T.useLive T.unequal sidePanelState

      localControls <- initialLocalControls
      -- ref to track automatic FA pausing
      -- If user pauses FA before auto is triggered, clear the timeoutId
      mFAPauseRef <- R.useRef Nothing


--    | Effects
--    |

      -- When graph is changed, cleanup the mFAPauseRef so that forceAtlas
      -- timeout is retriggered.
      R.useEffect' $ do
        case graphStage' of
          Graph.Init -> R.setRef mFAPauseRef Nothing
          _          -> pure unit

      -- Handle case when FA is paused from outside events, eg. the automatic timer.
      R.useEffect' $ Sigmax.handleForceAtlas2Pause sigmaRef forceAtlasState mFAPauseRef Graph.forceAtlas2Settings

      -- Handle automatic edge hiding when FA is running (to prevent flickering).
      -- TODO Commented temporarily: this breaks forceatlas rendering after reset
      -- NOTE This is a hack anyways. It's force atlas that should be fixed.
      R.useEffect2' sigmaRef forceAtlasState' $ do
        T.modify_ (SigmaxT.forceAtlasEdgeState forceAtlasState') showEdges

      -- Automatic opening of sidebar when a node is selected (but only first time).
      R.useEffect' $ do
        if sidePanelState' == GT.InitialClosed && (not Set.isEmpty selectedNodeIds') then do
          T.write_ GT.Opened sidePanelState
          T.write_ GET.SideTabData sideTab
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


--    | Computed
--    |

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

      let gap = H.span { className: "graph-toolbar__gap" } []

--    | Render
--    |

      pure $

        H.nav
        { className: "graph-toolbar" }
        [
          H.div
          { className: "flex-shrink-0" }
          [
            H.div
            { className: "d-flex" }
            [
              -- View Settings
              B.fieldset
              { className: "graph-toolbar__section"
              , titleSlot: H.text "View settings"
              }
              [
                -- change type button (?)
                centerButton sigmaRef
              ,
                gap
              ,
                edgesToggleButton
                { state: showEdges
                , stateAtlas: forceAtlasState
                }
              ,
                gap
              ,
                louvainToggleButton { state: showLouvain }
              ]
            ,
              -- Actions
              B.fieldset
              { className: "graph-toolbar__section"
              , titleSlot: H.text "Actions"
              }
              [
                -- resetForceAtlasButton { forceAtlasState, sigmaRef }
                pauseForceAtlasButton { state: forceAtlasState }
              ,
                gap
              ,
                cameraButton
                { id: graphId
                , hyperdataGraph: hyperdataGraph
                , session: session
                , sigmaRef: sigmaRef
                , reloadForest
                }
              ]
            ]
          ,
            -- Selection Settings
            B.fieldset
            { className: intercalate " "
                [ "graph-toolbar__section"
                , "graph-toolbar__section--selection"
                ]
            , titleSlot: H.text "Selection settings"
            }
            [
              -- zoom: 0 -100 - calculate ratio
              multiSelectEnabledButton { state: multiSelectEnabled }
            ,
              gap
            ,
              -- toggle multi node selection
              -- save button
              mouseSelectorSizeButton sigmaRef localControls.mouseSelectorSize
            ]
          ]
        ,
          -- Controls
          B.fieldset
          { className: intercalate " "
              [ "graph-toolbar__section"
              , "graph-toolbar__section--controls"
              , "flex-grow-1 flex-shrink-1"
              ]
          , titleSlot: H.text "Controls"
          }
          [
            H.div
            { className: "d-flex justify-content-between mb-3" }
            [
              edgeConfluenceControl
              { range: edgeConfluenceRange
              , state: edgeConfluence }
            ,
              edgeWeightControl
              { range: edgeWeightRange
              , state: edgeWeight }
            ]
          ,
            H.div
            { className: "d-flex justify-content-between" }
            [
              -- change level
              -- file upload
              -- run demo
              -- search button
              -- search topics
              labelSizeButton sigmaRef localControls.labelSize
            ,
              -- labels size: 1-4
              nodeSizeControl
              { range: nodeSizeRange
              , state: nodeSize }

            ]
          ]
        ]

          --   H.ul {} [ -- change type button (?)
          --     H.li {} [ centerButton sigmaRef ]
          --   , H.li {} [ pauseForceAtlasButton {state: forceAtlasState} ]
          --   , H.li {} [ edgesToggleButton {state: showEdges} ]
          --   , H.li {} [ louvainToggleButton showLouvain ]
          --   , H.li {} [ edgeConfluenceControl edgeConfluenceRange edgeConfluence ]
          --   , H.li {} [ edgeWeightControl edgeWeightRange edgeWeight ]
          --     -- change level
          --     -- file upload
          --     -- run demo
          --     -- search button
          --     -- search topics
          --   , H.li {} [ labelSizeButton sigmaRef localControls.labelSize ] -- labels size: 1-4
          --   , H.li {} [ nodeSizeControl nodeSizeRange nodeSize ]
          --     -- zoom: 0 -100 - calculate ratio
          --   , H.li {} [ multiSelectEnabledButton multiSelectEnabled ]  -- toggle multi node selection
          --     -- save button
          --   , H.li {} [ nodeSearchControl { graph: graph
          --                                  , multiSelectEnabled: multiSelectEnabled
          --                                  , selectedNodeIds: selectedNodeIds } ]
          --   , H.li {} [ mouseSelectorSizeButton sigmaRef localControls.mouseSelectorSize ]
          --   , H.li {} [ cameraButton { id: graphId
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
                    , sidePanel      :: T.Box (Maybe (Record GEST.SidePanel))
                    , sidePanelState :: T.Box GT.SidePanelState }
                 -> R.Hooks (Record Controls)
useGraphControls { forceAtlasS
                 , graph
                 , graphId
                 , hyperdataGraph
                 , reloadForest
                 , session
                 , sidePanel
                 , sidePanelState } = do
  edgeConfluence <- T.useBox $ Range.Closed { min: 0.0, max: 1.0 }
  edgeWeight <- T.useBox $ Range.Closed {
      min: 0.0
    , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph
    }
  forceAtlasState <- T.useBox forceAtlasS
  graphStage      <- T.useBox Graph.Init
  nodeSize <- T.useBox $ Range.Closed { min: 0.0, max: 100.0 }
  showEdges <- T.useBox SigmaxT.EShow
  showLouvain <- T.useBox false
  sigma <- Sigmax.initSigma
  sigmaRef <- R.useRef sigma

  { multiSelectEnabled, removedNodeIds, selectedNodeIds, showControls, sideTab } <- GEST.focusedSidePanel sidePanel

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
       , sideTab
       , sigmaRef
       , reloadForest
       }
