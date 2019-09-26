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

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple as DOM
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Timer (TimeoutId, setTimeout)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Button (centerButton)
import Gargantext.Components.GraphExplorer.RangeControl (edgeSizeControl, nodeSizeControl)
import Gargantext.Components.GraphExplorer.SlideButton (cursorSizeButton, labelSizeButton)
import Gargantext.Components.GraphExplorer.ToggleButton (edgesToggleButton, pauseForceAtlasButton)
import Gargantext.Hooks.Sigmax.Sigma as Sigma
import Gargantext.Hooks.Sigmax as Sigmax
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2


type Controls =
  ( cursorSize :: R.State Number
  , forceAtlas2Paused :: R.State Boolean
  , multiNodeSelect :: R.Ref Boolean
  , showControls :: R.State Boolean
  , showSidePanel :: R.State Boolean
  , showTree :: R.State Boolean
  , sigmaRef :: R.Ref (Maybe Sigmax.Sigma)
  )

controlsToSigmaSettings :: Record Controls -> Record Graph.SigmaSettings
controlsToSigmaSettings { cursorSize: (cursorSize /\ _)} = Graph.sigmaSettings

type LocalControls =
  ( edgeSize :: R.State Range.NumberRange
  , labelSize :: R.State Number
  , nodeSize :: R.State Range.NumberRange
  , showEdges :: R.State Boolean
  )

initialLocalControls :: R.Hooks (Record LocalControls)
initialLocalControls = do
  edgeSize <- R.useState' $ Range.Closed { min: 0.5, max: 1.0 }
  labelSize <- R.useState' 14.0
  nodeSize <- R.useState' $ Range.Closed { min: 5.0, max: 10.0 }
  showEdges <- R.useState' true

  pure $ {
    edgeSize
  , labelSize
  , nodeSize
  , showEdges
  }

controls :: Record Controls -> R.Element
controls props = R.createElement controlsCpt props []

controlsCpt :: R.Component Controls
controlsCpt = R.hooksComponent "GraphControls" cpt
  where
    forceAtlas2Timeout = 5.0  -- in seconds

    cpt props _ = do
      localControls <- initialLocalControls
      timeoutIdRef <- R.useRef (Nothing :: Maybe TimeoutId)

      R.useEffectOnce $ do
          timeoutId <- setTimeout (round $ forceAtlas2Timeout * 1000.0) $ do
            log2 "Pausing forceatlas" props.sigmaRef
            --let (pauseForceAtlas /\ setPauseForceAtlas) = localControls.pauseForceAtlas
            let (forceAtlas2Paused /\ setForceAtlas2Paused) = props.forceAtlas2Paused
            let mSigma = Sigmax.readSigma <$> R.readRef props.sigmaRef
            case mSigma of
              Just (Just s) -> if forceAtlas2Paused then do
                  pure unit
                else
                  setForceAtlas2Paused $ const true
                  --Sigma.stopForceAtlas2 s
              _             -> pure unit
            R.setRef timeoutIdRef Nothing
          R.setRef timeoutIdRef $ Just timeoutId
          pure $ pure unit

      pure $ case getShowControls props of
        false -> RH.div {} []
        true -> RH.div { className: "col-md-12", style: { paddingBottom: "10px" } }
            [ R2.menu { id: "toolbar" }
              [ RH.ul {}
                [ -- change type button (?)
                  RH.li {} [ centerButton props.sigmaRef ]
                , RH.li {} [ pauseForceAtlasButton timeoutIdRef props.forceAtlas2Paused ] -- spatialization (pause ForceAtlas2)
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
  cursorSize <- R.useState' 10.0
  forceAtlas2Paused <- R.useState' false
  multiNodeSelect <- R.useRef false
  showControls <- R.useState' false
  showSidePanel <- R.useState' false
  showTree <- R.useState' false
  sigmaRef <- R2.nothingRef

  pure {
    cursorSize
  , forceAtlas2Paused
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
setShowTree { showTree: ( _ /\ set ) } v = set $ const v

setCursorSize :: Record Controls -> Number -> Effect Unit
setCursorSize { cursorSize: ( _ /\ setSize ) } v = setSize $ const v

setMultiNodeSelect :: Record Controls -> Boolean -> Effect Unit
setMultiNodeSelect { multiNodeSelect } = R.setRef multiNodeSelect
