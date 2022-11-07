module Gargantext.Components.GraphExplorer.Toolbar.Controls
 ( controls
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
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Toolbar.Buttons (centerButton, cameraButton, edgesToggleButton, louvainToggleButton, pauseForceAtlasButton, multiSelectEnabledButton)
import Gargantext.Components.GraphExplorer.Toolbar.RangeControl (edgeConfluenceControl, edgeWeightControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.Toolbar.SlideButton (labelSizeButton, labelRenderedSizeThresholdButton, mouseSelectorSizeSlider)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Types as GT
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Toolbar.Controls"

type Controls =
  ( fa2Ref       :: R.Ref (Maybe ForceAtlas.FA2Layout)
  , reloadForest :: T2.ReloadS
  , sigmaRef     :: R.Ref Sigmax.Sigma
  )

controls :: R2.Leaf Controls
controls = R2.leaf controlsCpt
controlsCpt :: R.Memo Controls
controlsCpt = R.memo' $ here.component "controls" cpt where
  cpt { fa2Ref
      , reloadForest
      , sigmaRef
      } _ = do
    -- | States
    -- |
    { edgeConfluence
    , edgeWeight
    , forceAtlasState
    , graph
    , graphId
    , graphStage
    , hyperdataGraph
    , labelRenderedSizeThreshold
    , labelSize
    , mouseSelectorSize
    , multiSelectEnabled
    , nodeSize
    , selectedNodeIds
    , showEdges
    , showLouvain
    , showSidebar
    , sideTab
    } <- GraphStore.use

    forceAtlasState'    <- R2.useLive' forceAtlasState
    graph'              <- R2.useLive' graph
    graphId'            <- R2.useLive' graphId
    graphStage'         <- R2.useLive' graphStage
    hyperdataGraph'     <- R2.useLive' hyperdataGraph
    selectedNodeIds'    <- R2.useLive' selectedNodeIds
    showSidebar'        <- R2.useLive' showSidebar

    session <- useSession

    -- ref to track automatic FA pausing
    -- If user pauses FA before auto is triggered, clear the timeoutId
    mFAPauseRef <- R.useRef Nothing


    -- | Effects
    -- |

    -- When graph is changed, cleanup the mFAPauseRef so that forceAtlas
    -- timeout is retriggered.
    R.useEffect' $ do
      case graphStage' of
        GET.Init -> R.setRef mFAPauseRef Nothing
        _          -> pure unit

    -- Handle case when FA is paused from outside events, eg. the automatic timer.
    R.useEffect' $ Sigmax.handleForceAtlas2Pause fa2Ref forceAtlasState mFAPauseRef Graph.forceAtlas2Settings

    -- Handle automatic edge hiding when FA is running (to prevent flickering).
    -- TODO Commented temporarily: this breaks forceatlas rendering after reset
    -- NOTE This is a hack anyways. It's force atlas that should be fixed.
    R.useEffect2' sigmaRef forceAtlasState' $ do
      T.modify_ (SigmaxT.forceAtlasEdgeState forceAtlasState') showEdges
      let renderLabels = SigmaxT.forceAtlasLabelState forceAtlasState'
      here.log2 "[controls] renderLabels" renderLabels
      Sigmax.dependOnSigma (R.readRef sigmaRef) "[graphCpt (Cleanup)] no sigma" $ \sigma -> do
          Sigma.setSettings sigma { renderLabels }
      -- v <- T.read showEdges
      -- here.log2 "[controls] modifed showEdges to forceAtlasState'" v

    -- Automatic opening of sidebar when a node is selected (but only first time).
    R.useEffect' $ do
      if showSidebar' == GT.InitialClosed && (not Set.isEmpty selectedNodeIds') then do
        T.write_ GT.Opened showSidebar
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


    -- | Computed
    -- |

    let edgesConfluenceSorted = A.sortWith (_.confluence) $ Seq.toUnfoldable $ SigmaxT.graphEdges graph'
    let edgeConfluenceMin = maybe 0.0 _.confluence $ A.head edgesConfluenceSorted
    let edgeConfluenceMax = maybe 100.0 _.confluence $ A.last edgesConfluenceSorted
    let edgeConfluenceRange = Range.Closed { min: edgeConfluenceMin, max: edgeConfluenceMax }

    --let edgesWeightSorted = A.sortWith (_.weight) $ Seq.toUnfoldable $ SigmaxT.graphEdges graph
    --let edgeWeightMin = maybe 0.0 _.weight $ A.head edgesWeightSorted
    --let edgeWeightMax = maybe 100.0 _.weight $ A.last edgesWeightSorted
    --let edgeWeightRange = Range.Closed { min: edgeWeightMin, max: edgeWeightMax }
    let edgeWeightRange = Range.Closed {
          min: 0.0
        , max: I.toNumber $ Seq.length $ SigmaxT.graphEdges graph'
        }

    let nodesSorted = A.sortWith (_.size) $ Seq.toUnfoldable $ SigmaxT.graphNodes graph'
    let nodeSizeMin = maybe 0.0 _.size $ A.head nodesSorted
    let nodeSizeMax = maybe 100.0 _.size $ A.last nodesSorted
    let nodeSizeRange = Range.Closed { min: nodeSizeMin, max: nodeSizeMax }

    let gap = H.span { className: "graph-toolbar__gap" } []

    -- | Render
    -- |

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
{-            ,
              cameraButton
              { id: graphId'
              , forceAtlasState
              , hyperdataGraph: hyperdataGraph'
              , reloadForest
              , session: session
              , sigmaRef: sigmaRef
              }
-}
            ]
          ,
            -- View Settings
            B.fieldset
            { className: "graph-toolbar__section"
            , titleSlot: H.text "View settings"
            }
            [
              centerButton { forceAtlasState
                           , sigmaRef }
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
              louvainToggleButton { forceAtlasState
                                  , state: showLouvain }
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
            multiSelectEnabledButton { forceAtlasState
                                     , state: multiSelectEnabled }
          ,
            gap
          ,
            -- toggle multi node selection
            -- save button
            mouseSelectorSizeSlider { forceAtlasState
                                    , sigmaRef
                                    , state: mouseSelectorSize }
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
            { forceAtlasState
            , range: edgeConfluenceRange
            , state: edgeConfluence }
          {- ,
            edgeWeightControl
            { forceAtlasState
            , range: edgeWeightRange
            , state: edgeWeight }
          -}
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
            labelSizeButton { forceAtlasState
                            , sigmaRef
                            , state: labelSize }
          ,
            -- labels size: 1-4
            nodeSizeControl
            { forceAtlasState
            , range: nodeSizeRange
            , state: nodeSize }

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
            labelRenderedSizeThresholdButton { forceAtlasState
                                             , sigmaRef
                                             , state: labelRenderedSizeThreshold }
          -- ,
          --   -- labels size: 1-4
          --   nodeSizeControl
          --   { range: nodeSizeRange
          --   , state: nodeSize }

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
