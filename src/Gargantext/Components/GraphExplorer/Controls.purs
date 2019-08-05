module Gargantext.Components.GraphExplorer.Controls
 ( Controls, useGraphControls, controls, controlsCpt
 , getShowTree, setShowTree
 , getShowControls, setShowControls
 , getShowSidePanel, setShowSidePanel
 , getShowEdges, setShowEdges
 , getCursorSize, setCursorSize
 , getLabelSize, setLabelSize
 , getNodeSize, setNodeSize
 , getMultiNodeSelect, setMultiNodeSelect
 ) where

import Reactix as R
import Reactix.DOM.HTML as RH


type Controls =
  ( showTree :: R.State Boolean
  , showControls :: R.State Boolean
  , showSidePanel :: R.State Boolean
  , showEdges :: R.State Boolean
  , cursorSize :: R.State Number
  , labelSize :: R.State Number
  , nodeSize :: R.State Number
  , multiNodeSelect :: R.Ref Boolean
  )

controls = Record Controls -> R.Element
controls props = R.createElement controlsCpt props []

controlsCpt :: R.Component Controls
controlsCpt = R.hooksComponent "GraphControls" cpt
  where
    cpt props _ =
      case getShowControls props of
        false -> pure $ RH.div {} []
        true -> do
          RH.div { className: "col-md-12", style: { paddingBottom: "10px" } }
          [ menu { id: "toolbar" }
            [ RH.ul {}
              [ -- change type button (?)
                RH.li {} [ edgesToggleButton props.showEdges ]
                -- change level
                -- file upload
                -- run demo
                -- search button
                -- search topics
                -- cursor size: 0-100
                -- labels size: 1-4
                -- node size : 5-15
                -- edge size : ??
                -- zoom: 0 -100 - calculate ratio
                -- toggle multi node selection
                -- spatialization (pause ForceAtlas2)
                -- save button
              ]
            ]
          ]

useGraphControls :: R.Hooks (Record Controls)
useGraphControls = do
  showTree <- R.useState' false
  showControls <- R.useState' false
  showSidePanel <- R.useState' false
  showEdges <- R.useState' true
  cursorSize <- R.useState' 10
  labelSize <- R.useState' 3
  nodeSize <- R.useState' 10
  multiNodeSelect <- R.useRef false
  pure { showTree, showControls, showSidePanel, showEdges, cursorSize, labelSize, nodeSize, multiNodeSelect }
  
getShowTree :: Controls -> Boolean
getShowTree { showTree: ( should /\ _ ) } = should

getShowControls :: Controls -> Boolean
getShowControls { showControls: ( should /\ _ ) } = should

getShowSidePanel :: Controls -> Boolean
getShowSidePanel { showSidePanel: ( should /\ _ ) } = should

getShowEdges :: Controls -> Boolean
getShowEdges { showEdges: ( should /\ _ ) } = should 

getCursorSize :: Controls -> Number
getCursorSize { cursorSize: ( size /\ _ ) } = size

getLabelSize :: Controls -> Number
getLabelSize { labelSize: ( size /\ _ ) } = size

getNodeSize :: Controls -> Number
getNodeSize { nodeSize: ( size /\ _ ) } = size

getMultiNodeSelect :: Controls -> Boolean
getMultiNodeSelect { multiNodeSelect } = R.readRef multiNodeSelect

setShowTree :: Controls -> Boolean -> Effect Unit
setShowTree { showTree: ( _ /\ set ) } = set

setShowControls :: Controls -> Boolean -> Effect Unit
setShowControls { showControls: ( _ /\ set ) } = set

setShowSidePanel :: Controls -> Boolean -> Effect Unit
setShowSidePanel { showSidePanel: ( _ /\ set ) } = set

setShowEdges :: Controls -> Boolean -> Effect Unit
setShowEdges { showEdges: ( _ /\ set ) } = set

setCursorSize :: Controls -> Number -> Effect Unit
setCursorSize { cursorSize: ( _ /\ setSize ) } = setSize

setLabelSize :: Controls -> Number -> Effect Unit
setLabelSize { labelSize: ( _ /\ setSize) } = setSize

setNodeSize :: Controls -> Number -> Effect Unit
setNodeSize { nodeSize: ( _ /\ setSize ) } = setSize

setMultiNodeSelect :: Controls -> Boolean -> Effect Unit
setMultiNodeSelect { multiNodeSelect } = R.setRef multiNodeSelect
