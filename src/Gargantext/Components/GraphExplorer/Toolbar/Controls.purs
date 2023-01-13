module Gargantext.Components.GraphExplorer.Toolbar.Controls
 ( controls
 ) where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Timer (setTimeout)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphExplorer.Resources as Graph
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Toolbar.Buttons (cameraButton, centerButton, edgesToggleButton, louvainButton, pauseForceAtlasButton, pauseNoverlapButton, multiSelectEnabledButton)
import Gargantext.Components.GraphExplorer.Toolbar.RangeControl (edgeConfluenceControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.Toolbar.SlideButton (labelSizeButton, labelRenderedSizeThresholdButton, mouseSelectorSizeSlider)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.ForceAtlas2 as ForceAtlas
import Gargantext.Hooks.Sigmax.Noverlap as Noverlap
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Toolbar.Controls"

type Controls =
  ( fa2Ref       :: R.Ref (Maybe ForceAtlas.FA2Layout)
  , noverlapRef  :: R.Ref (Maybe Noverlap.NoverlapLayout)
  , reloadForest :: T2.ReloadS
  , sigmaRef     :: R.Ref Sigmax.Sigma
  )

controls :: R2.Leaf Controls
controls = R2.leaf controlsCpt
controlsCpt :: R.Memo Controls
controlsCpt = R.memo' $ here.component "controls" cpt where
  cpt { fa2Ref
      , noverlapRef
      , reloadForest
      , sigmaRef
      } _ = do
    -- | States
    -- |
    { edgeConfluence
    , edgeConfluenceRange
    -- , edgeWeight
    , forceAtlasState
    , noverlapState
    , graph
    , graphId
    , graphStage
    , hyperdataGraph
    , labelRenderedSizeThreshold
    , labelSize
    , mouseSelectorSize
    , multiSelectEnabled
    , nodeSize
    , nodeSizeRange
    , selectedNodeIds
    , showEdges
    , showSidebar
    , sideTab
    } <- GraphStore.use

    graphId'              <- R2.useLive' graphId
    hyperdataGraph'       <- R2.useLive' hyperdataGraph
    forceAtlasState'      <- R2.useLive' forceAtlasState
    noverlapState'        <- R2.useLive' noverlapState
    graphStage'           <- R2.useLive' graphStage
    selectedNodeIds'      <- R2.useLive' selectedNodeIds
    showSidebar'          <- R2.useLive' showSidebar
    edgeConfluenceRange'  <- R2.useLive' edgeConfluenceRange
    nodeSizeRange'        <- R2.useLive' nodeSizeRange

    -- ref to track automatic FA pausing
    -- If user pauses FA before auto is triggered, clear the timeoutId
    mFAPauseRef <- R.useRef Nothing


    -- | Effects
    -- |

    -- When graph is changed, cleanup the mFAPauseRef so that forceAtlas
    -- timeout is retriggered.
    R.useEffect1' graphStage' $ do
      case graphStage' of
        GET.Init -> R.setRef mFAPauseRef Nothing
        _          -> pure unit

    -- Handle case when FA is paused from outside events, eg. the automatic timer.
    R.useEffect' $ Sigmax.handleForceAtlas2Pause fa2Ref forceAtlasState mFAPauseRef Graph.forceAtlas2Settings

    R.useEffect' do
      here.log2 "[controls] noverlapState'" noverlapState'
      case R.readRef noverlapRef of
        Nothing -> pure unit
        Just noverlap -> do
          case noverlapState' of
            SigmaxT.NoverlapRunning -> do
              Noverlap.start noverlap
            SigmaxT.NoverlapPaused -> do
              Noverlap.stop noverlap

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
      case forceAtlasState' of
        SigmaxT.InitialRunning -> do
          timeoutId <- setTimeout 9000 $ do
            case forceAtlasState' of
              SigmaxT.InitialRunning ->
                T.write_ SigmaxT.Paused forceAtlasState
              _ -> pure unit
            R.setRef mFAPauseRef Nothing
          R.setRef mFAPauseRef $ Just timeoutId
          pure unit
        _ -> pure unit


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
            , pauseNoverlapButton { state: noverlapState }
            ,
              gap
            ,
              cameraButton
              { id: graphId'
              , forceAtlasState
              , hyperdataGraph: hyperdataGraph'
              , reloadForest
              , sigmaRef: sigmaRef
              }
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
              louvainButton { forceAtlasState
                            , graph
                            , sigmaRef }
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
          B.wad
          [ "d-flex",  "gap-6", "px-1" ]
          [
            B.wad
            [ "d-flex", "flex-column", "flex-grow-1", "pt-1", "gap-4" ]
            [
              edgeConfluenceControl
              { forceAtlasState
              , range: edgeConfluenceRange'
              , state: edgeConfluence }
            {- ,
              edgeWeightControl
              { forceAtlasState
              , range: edgeWeightRange
              , state: edgeWeight }
            -}
            ,
              nodeSizeControl
              { forceAtlasState
              , range: nodeSizeRange'
              , state: nodeSize
              }
            ]
          ,
            B.wad
            [ "d-flex", "flex-column", "flex-grow-1", "pt-1", "gap-4" ]
            [
              labelSizeButton
              { forceAtlasState
              , graph
              , sigmaRef
              , state: labelSize
              }
            ,
              labelRenderedSizeThresholdButton
              { forceAtlasState
              , sigmaRef
              , state: labelRenderedSizeThreshold
              }
              -- ,
              --   nodeSizeControl
              --   { range: nodeSizeRange
              --   , state: nodeSize
              --   }
            ]
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
