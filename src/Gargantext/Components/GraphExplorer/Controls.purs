module Gargantext.Components.GraphExplorer.Controls
 ( Controls
 , controlsToSigmaSettings
 , useGraphControls
 , controls
 , controlsCpt
 , getShowTree, setShowTree
 , getShowControls, setShowControls
 , getCursorSize, setCursorSize
 ) where

import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Timer (setTimeout)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Button (centerButton)
import Gargantext.Components.GraphExplorer.RangeControl (edgeConfluenceControl, edgeWeightControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.Search (nodeSearchControl)
import Gargantext.Components.GraphExplorer.SlideButton (cursorSizeButton, labelSizeButton)
import Gargantext.Components.GraphExplorer.ToggleButton (multiSelectEnabledButton, edgesToggleButton, pauseForceAtlasButton)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

type Controls =
  ( cursorSize      :: R.State Number
  , edgeConfluence :: R.State Range.NumberRange
  , edgeWeight :: R.State Range.NumberRange
  , forceAtlasState :: R.State SigmaxTypes.ForceAtlasState
  , graph           :: SigmaxTypes.SGraph
  , graphStage      :: R.State Graph.Stage
  , multiSelectEnabled :: R.State Boolean
  , nodeSize        :: R.State Range.NumberRange
  , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
  , showControls    :: R.State Boolean
  , showEdges       :: R.State SigmaxTypes.ShowEdgesState
  , showSidePanel   :: R.State GET.SidePanelState
  , showTree        :: R.State Boolean
  , sigmaRef        :: R.Ref Sigmax.Sigma
  )

controlsToSigmaSettings :: Record Controls -> Record Graph.SigmaSettings
controlsToSigmaSettings { cursorSize: (cursorSize /\ _)} = Graph.sigmaSettings

type LocalControls =
  ( labelSize :: R.State Number
  )

initialLocalControls :: R.Hooks (Record LocalControls)
initialLocalControls = do
  labelSize <- R.useState' 14.0
  search <- R.useState' ""

  pure $ {
    labelSize
  }

controls :: Record Controls -> R.Element
controls props = R.createElement controlsCpt props []

controlsCpt :: R.Component Controls
controlsCpt = R.hooksComponent "GraphControls" cpt
  where
    cpt props _ = do
      localControls <- initialLocalControls
      -- ref to track automatic FA pausing
      -- If user pauses FA before auto is triggered, clear the timeoutId
      mFAPauseRef <- R.useRef Nothing

      -- When graph is changed, cleanup the mFAPauseRef so that forceAtlas
      -- timeout is retriggered.
      R.useEffect' $ do
        case fst props.graphStage of
          Graph.Init -> R.setRef mFAPauseRef Nothing
          _          -> pure unit

      -- Handle case when FA is paused from outside events, eg. the automatic timer.
      R.useEffect' $ Sigmax.handleForceAtlas2Pause props.sigmaRef props.forceAtlasState mFAPauseRef

      -- Handle automatic edge hiding when FA is running (to prevent flickering).
      R.useEffect2' props.sigmaRef props.forceAtlasState $
        snd props.showEdges $ SigmaxTypes.forceAtlasEdgeState (fst props.forceAtlasState)

      -- Automatic opening of sidebar when a node is selected (but only first time).
      R.useEffect' $ do
        if fst props.showSidePanel == GET.InitialClosed && (not Set.isEmpty $ fst props.selectedNodeIds) then
          snd props.showSidePanel $ \_ -> GET.Opened
        else
          pure unit

      -- Timer to turn off the initial FA. This is because FA eats up lot of
      -- CPU, has memory leaks etc.
      R.useEffect1' (fst props.forceAtlasState) $ do
        if (fst props.forceAtlasState) == SigmaxTypes.InitialRunning then do
          timeoutId <- setTimeout 2000 $ do
            let (toggled /\ setToggled) = props.forceAtlasState
            case toggled of
              SigmaxTypes.InitialRunning -> setToggled $ const SigmaxTypes.Paused
              _ -> pure unit
            R.setRef mFAPauseRef Nothing
          R.setRef mFAPauseRef $ Just timeoutId
          pure unit
         else
           pure unit

      let edgesConfluenceSorted = A.sortWith (_.confluence) $ Seq.toUnfoldable $ SigmaxTypes.graphEdges props.graph
      let edgeConfluenceMin = maybe 0.0 _.confluence $ A.head edgesConfluenceSorted
      let edgeConfluenceMax = maybe 100.0 _.confluence $ A.last edgesConfluenceSorted
      let edgeConfluenceRange = Range.Closed { min: edgeConfluenceMin, max: edgeConfluenceMax }

      let edgesWeightSorted = A.sortWith (_.weight) $ Seq.toUnfoldable $ SigmaxTypes.graphEdges props.graph
      let edgeWeightMin = maybe 0.0 _.weight $ A.head edgesWeightSorted
      let edgeWeightMax = maybe 100.0 _.weight $ A.last edgesWeightSorted
      let edgeWeightRange = Range.Closed { min: edgeWeightMin, max: edgeWeightMax }

      let nodesSorted = A.sortWith (_.size) $ Seq.toUnfoldable $ SigmaxTypes.graphNodes props.graph
      let nodeSizeMin = maybe 0.0 _.size $ A.head nodesSorted
      let nodeSizeMax = maybe 100.0 _.size $ A.last nodesSorted
      let nodeSizeRange = Range.Closed { min: nodeSizeMin, max: nodeSizeMax }

      pure $ case getShowControls props of
        false -> RH.div {} []
        true -> RH.div { className: "col-md-12", style: { paddingBottom: "10px" } }
            [ R2.menu { id: "toolbar" }
              [ RH.ul {}
                [ -- change type button (?)
                  RH.li {} [ centerButton props.sigmaRef ]
                , RH.li {} [ pauseForceAtlasButton {state: props.forceAtlasState} ]
                , RH.li {} [ edgesToggleButton {state: props.showEdges} ]
                , RH.li {} [ edgeConfluenceControl edgeConfluenceRange props.edgeConfluence ]
                , RH.li {} [ edgeWeightControl edgeWeightRange props.edgeWeight ]
                  -- change level
                  -- file upload
                  -- run demo
                  -- search button
                  -- search topics
                , RH.li {} [ cursorSizeButton props.cursorSize ] -- cursor size: 0-100
                , RH.li {} [ labelSizeButton props.sigmaRef localControls.labelSize ] -- labels size: 1-4
                , RH.li {} [ nodeSizeControl nodeSizeRange props.nodeSize ]
                  -- zoom: 0 -100 - calculate ratio
                , RH.li {} [ multiSelectEnabledButton props.multiSelectEnabled ]  -- toggle multi node selection
                  -- save button
                , RH.li {} [ nodeSearchControl { selectedNodeIds: props.selectedNodeIds } ]
                ]
              ]
            ]

useGraphControls :: SigmaxTypes.SGraph -> R.Hooks (Record Controls)
useGraphControls graph = do
  cursorSize      <- R.useState' 10.0
  edgeConfluence <- R.useState' $ Range.Closed { min: 0.0, max: 1.0 }
  edgeWeight <- R.useState' $ Range.Closed { min: 0.0, max: 1.0 }
  forceAtlasState <- R.useState' SigmaxTypes.InitialRunning
  graphStage      <- R.useState' Graph.Init
  multiSelectEnabled <- R.useState' false
  nodeSize <- R.useState' $ Range.Closed { min: 0.0, max: 100.0 }
  showTree <- R.useState' false
  selectedNodeIds <- R.useState' $ Set.empty
  showControls    <- R.useState' false
  showEdges <- R.useState' SigmaxTypes.EShow
  showSidePanel   <- R.useState' GET.InitialClosed
  sigma <- Sigmax.initSigma
  sigmaRef <- R.useRef sigma

  pure { cursorSize
       , edgeConfluence
       , edgeWeight
       , forceAtlasState
       , graph
       , graphStage
       , multiSelectEnabled
       , nodeSize
       , selectedNodeIds
       , showControls
       , showEdges
       , showSidePanel
       , showTree
       , sigmaRef
       }

getShowControls :: Record Controls -> Boolean
getShowControls { showControls: ( should /\ _ ) } = should

getShowTree :: Record Controls -> Boolean
getShowTree { showTree: ( should /\ _ ) } = should

getCursorSize :: Record Controls -> Number
getCursorSize { cursorSize: ( size /\ _ ) } = size

setShowControls :: Record Controls -> Boolean -> Effect Unit
setShowControls { showControls: ( _ /\ set ) } v = set $ const v

setShowTree :: Record Controls -> Boolean -> Effect Unit
setShowTree { showTree: ( _ /\ set ) } v = set $ not <<< const v

setCursorSize :: Record Controls -> Number -> Effect Unit
setCursorSize { cursorSize: ( _ /\ setSize ) } v = setSize $ const v
