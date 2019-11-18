module Gargantext.Components.GraphExplorer.Controls
 ( Controls
 , controlsToSigmaSettings
 , useGraphControls
 , controls
 , controlsCpt
 , getShowTree, setShowTree
 , getShowControls, setShowControls
 , getShowSidePanel, setShowSidePanel
 , getCursorSize, setCursorSize
 , getMultiNodeSelect, setMultiNodeSelect
 ) where

import Data.Maybe (Maybe(..))
import DOM.Simple.Console (log, log2)
import Data.Tuple.Nested ((/\), get1)
import Effect (Effect)
import Effect.Timer (clearTimeout, setTimeout)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Button (centerButton)
import Gargantext.Components.GraphExplorer.RangeControl (edgeSizeControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.SlideButton (cursorSizeButton, labelSizeButton)
import Gargantext.Components.GraphExplorer.ToggleButton (edgesToggleButton, pauseForceAtlasButton)
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2


type Controls =
  ( cursorSize      :: R.State Number
  , multiNodeSelect :: R.Ref Boolean
  , showControls    :: R.State Boolean
  , showSidePanel   :: R.State Boolean
  , showTree        :: R.State Boolean
  , sigmaRef        :: R.Ref Sigmax.Sigma
  )

controlsToSigmaSettings :: Record Controls -> Record Graph.SigmaSettings
controlsToSigmaSettings { cursorSize: (cursorSize /\ _)} = Graph.sigmaSettings

type LocalControls =
  ( edgeSize :: R.State Range.NumberRange
  , labelSize :: R.State Number
  , nodeSize :: R.State Range.NumberRange
  , pauseForceAtlas :: R.State Boolean
  , showEdges :: R.State Boolean
  )

initialLocalControls :: R.Hooks (Record LocalControls)
initialLocalControls = do
  edgeSize <- R.useState' $ Range.Closed { min: 0.5, max: 1.0 }
  labelSize <- R.useState' 14.0
  nodeSize <- R.useState' $ Range.Closed { min: 5.0, max: 10.0 }
  pauseForceAtlas <- R.useState' true
  showEdges <- R.useState' true

  pure $ {
    edgeSize
  , labelSize
  , nodeSize
  , pauseForceAtlas
  , showEdges
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
      -- TODO: mFAPauseRef needs to be set higher up the tree
      --mFAPauseRef <- R.useRef Nothing

      --R.useEffect $ handleForceAtlasPause props.sigmaRef localControls.pauseForceAtlas mFAPauseRef
      R.useEffect' $ Sigmax.handleForceAtlas2Pause props.sigmaRef localControls.pauseForceAtlas $ get1 localControls.showEdges

      R.useEffectOnce' $ do
        timeoutId <- setTimeout 2000 $ do
          --R.setRef mFAPauseRef Nothing
          let (toggled /\ setToggled) = localControls.pauseForceAtlas
          if toggled then
            setToggled $ const false
          else
            pure unit
        --R.setRef mFAPauseRef $ Just timeoutId
        pure unit

      pure $ case getShowControls props of
        false -> RH.div {} []
        true -> RH.div { className: "col-md-12", style: { paddingBottom: "10px" } }
            [ R2.menu { id: "toolbar" }
              [ RH.ul {}
                [ -- change type button (?)
                  RH.li {} [ centerButton props.sigmaRef ]
                , RH.li {} [ pauseForceAtlasButton props.sigmaRef localControls.pauseForceAtlas ] -- spatialization (pause ForceAtlas2)
                , RH.li {} [ edgesToggleButton props.sigmaRef localControls.showEdges ]
                , RH.li {} [ edgeSizeControl props.sigmaRef localControls.edgeSize ] -- edge size : 0-3
                  -- change level
                  -- file upload
                  -- run demo
                  -- search button
                  -- search topics
                , RH.li {} [ cursorSizeButton props.cursorSize ] -- cursor size: 0-100
                , RH.li {} [ labelSizeButton props.sigmaRef localControls.labelSize ] -- labels size: 1-4
                , RH.li {} [ nodeSizeControl props.sigmaRef localControls.nodeSize ] -- node size : 5-15
                  -- zoom: 0 -100 - calculate ratio
                  -- toggle multi node selection
                  -- save button
                ]
              ]
            ]

useGraphControls :: R.Hooks (Record Controls)
useGraphControls = do
  cursorSize      <- R.useState' 10.0
  multiNodeSelect <- R.useRef false
  showControls    <- R.useState' false
  showSidePanel   <- R.useState' false
  showTree <- R.useState' false
  sigma <- Sigmax.initSigma
  sigmaRef <- R.useRef sigma

  pure { cursorSize
       , multiNodeSelect
       , showControls
       , showSidePanel
       , showTree
       , sigmaRef
       }

getShowControls :: Record Controls -> Boolean
getShowControls { showControls: ( should /\ _ ) } = should

getShowSidePanel :: Record Controls -> Boolean
getShowSidePanel { showSidePanel: ( should /\ _ ) } = should

getShowTree :: Record Controls -> Boolean
getShowTree { showTree: ( should /\ _ ) } = should

getCursorSize :: Record Controls -> Number
getCursorSize { cursorSize: ( size /\ _ ) } = size

getMultiNodeSelect :: Record Controls -> Boolean
getMultiNodeSelect { multiNodeSelect } = R.readRef multiNodeSelect

setShowControls :: Record Controls -> Boolean -> Effect Unit
setShowControls { showControls: ( _ /\ set ) } v = set $ const v

setShowSidePanel :: Record Controls -> Boolean -> Effect Unit
setShowSidePanel { showSidePanel: ( _ /\ set ) } v = set $ const v

setShowTree :: Record Controls -> Boolean -> Effect Unit
setShowTree { showTree: ( _ /\ set ) } v = set $ not <<< const v

setCursorSize :: Record Controls -> Number -> Effect Unit
setCursorSize { cursorSize: ( _ /\ setSize ) } v = setSize $ const v

setMultiNodeSelect :: Record Controls -> Boolean -> Effect Unit
setMultiNodeSelect { multiNodeSelect } = R.setRef multiNodeSelect
