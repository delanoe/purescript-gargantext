module Gargantext.Components.GraphExplorer.Controls
 ( Controls
 , controlsToSigmaSettings
 , useGraphControls
 , controls
 , controlsCpt
 , getShowTree, setShowTree
 , getShowControls, setShowControls
 , getShowSidePanel, setShowSidePanel
 , getShowEdges, setShowEdges
 , getCursorSize, setCursorSize
 , getLabelSize, setLabelSize
 , getNodeSize, setNodeSize
 , getMultiNodeSelect, setMultiNodeSelect
 ) where

import Data.Maybe (Maybe(..))
import DOM.Simple as DOM
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.RangeControl (edgeSizeControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.SlideButton (cursorSizeButton, labelSizeButton)
import Gargantext.Components.GraphExplorer.ToggleButton (edgesToggleButton, pauseForceAtlasButton)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2


type Controls =
  ( cursorSize :: R.State Number
  , edgeSize :: R.State Range.NumberRange
  , labelSize :: R.State Number
  , nodeSize :: R.State Range.NumberRange
  , multiNodeSelect :: R.Ref Boolean
  , pauseForceAtlas :: R.State Boolean
  , showControls :: R.State Boolean
  , showEdges :: R.State Boolean
  , showSidePanel :: R.State Boolean
  , showTree :: R.State Boolean
  , sigmaRef :: R.Ref (Maybe Sigmax.Sigma)
  )

controlsToSigmaSettings :: Record Controls -> Record Graph.SigmaSettings
controlsToSigmaSettings { cursorSize: (cursorSize /\ _)
                        , edgeSize: (Range.Closed { min: edgeSizeMin, max: edgeSizeMax } /\ _)
                        , labelSize: (labelSize /\ _)
                        , nodeSize: (Range.Closed { min: nodeSizeMin, max: nodeSizeMax } /\ _)
                        , showEdges: (showEdges /\ _)} = Graph.sigmaSettings {
    defaultLabelSize = labelSize
  , drawEdges = showEdges
  , drawEdgeLabels = showEdges
  , hideEdgesOnMove = not showEdges
  , minEdgeSize = if showEdges then edgeSizeMin else 0.0
  , maxEdgeSize = if showEdges then edgeSizeMax else 0.0
  , minNodeSize = nodeSizeMin
  , maxNodeSize = nodeSizeMax
  }

controls :: Record Controls -> R.Element
controls props = R.createElement controlsCpt props []

controlsCpt :: R.Component Controls
controlsCpt = R.hooksComponent "GraphControls" cpt
  where
    cpt props _ =
      case getShowControls props of
        false -> pure $ RH.div {} []
        true -> do
          pure $ RH.div { className: "col-md-12", style: { paddingBottom: "10px" } }
            [ R2.menu { id: "toolbar" }
              [ RH.ul {}
                [ -- change type button (?)
                  RH.li {} [ pauseForceAtlasButton props.sigmaRef props.pauseForceAtlas ] -- spatialization (pause ForceAtlas2)
                , RH.li {} [ edgesToggleButton props.showEdges ]
                , RH.li {} [ edgeSizeControl props.edgeSize ] -- edge size : 0-3
                  -- change level
                  -- file upload
                  -- run demo
                  -- search button
                  -- search topics
                , RH.li {} [ cursorSizeButton props.cursorSize ] -- cursor size: 0-100
                , RH.li {} [ labelSizeButton props.labelSize ] -- labels size: 1-4
                , RH.li {} [ nodeSizeControl props.nodeSize ] -- node size : 5-15
                  -- zoom: 0 -100 - calculate ratio
                  -- toggle multi node selection
                  -- save button
                ]
              ]
            ]

useGraphControls :: R.Hooks (Record Controls)
useGraphControls = do
  cursorSize <- R.useState' 10.0
  edgeSize <- R.useState' $ Range.Closed { min: 0.5, max: 1.0 }
  labelSize <- R.useState' 3.0
  nodeSize <- R.useState' $ Range.Closed { min: 5.0, max: 10.0 }
  multiNodeSelect <- R.useRef false
  pauseForceAtlas <- R.useState' true
  showControls <- R.useState' false
  showEdges <- R.useState' true
  showSidePanel <- R.useState' false
  showTree <- R.useState' false
  sigmaRef <- R.useRef Nothing

  pure {
      cursorSize
    , edgeSize
    , labelSize
    , multiNodeSelect
    , nodeSize
    , pauseForceAtlas
    , showControls
    , showEdges
    , showSidePanel
    , showTree
    , sigmaRef
    }

getShowTree :: Record Controls -> Boolean
getShowTree { showTree: ( should /\ _ ) } = should

getShowControls :: Record Controls -> Boolean
getShowControls { showControls: ( should /\ _ ) } = should

getShowSidePanel :: Record Controls -> Boolean
getShowSidePanel { showSidePanel: ( should /\ _ ) } = should

getShowEdges :: Record Controls -> Boolean
getShowEdges { showEdges: ( should /\ _ ) } = should

getCursorSize :: Record Controls -> Number
getCursorSize { cursorSize: ( size /\ _ ) } = size

getLabelSize :: Record Controls -> Number
getLabelSize { labelSize: ( size /\ _ ) } = size

getNodeSize :: Record Controls -> Range.NumberRange
getNodeSize { nodeSize: ( size /\ _ ) } = size

getMultiNodeSelect :: Record Controls -> Boolean
getMultiNodeSelect { multiNodeSelect } = R.readRef multiNodeSelect

setShowTree :: Record Controls -> Boolean -> Effect Unit
setShowTree { showTree: ( _ /\ set ) } v = set $ const v

setShowControls :: Record Controls -> Boolean -> Effect Unit
setShowControls { showControls: ( _ /\ set ) } v = set $ const v

setShowSidePanel :: Record Controls -> Boolean -> Effect Unit
setShowSidePanel { showSidePanel: ( _ /\ set ) } v = set $ const v

setShowEdges :: Record Controls -> Boolean -> Effect Unit
setShowEdges { showEdges: ( _ /\ set ) } v = set $ const v

setCursorSize :: Record Controls -> Number -> Effect Unit
setCursorSize { cursorSize: ( _ /\ setSize ) } v = setSize $ const v

setLabelSize :: Record Controls -> Number -> Effect Unit
setLabelSize { labelSize: ( _ /\ setSize) } v = setSize $ const v

setNodeSize :: Record Controls -> Range.NumberRange -> Effect Unit
setNodeSize { nodeSize: ( _ /\ setSize ) } v = setSize $ const v

setMultiNodeSelect :: Record Controls -> Boolean -> Effect Unit
setMultiNodeSelect { multiNodeSelect } = R.setRef multiNodeSelect
