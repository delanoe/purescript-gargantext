module Gargantext.Components.GraphExplorer.Sigmajs where

import Prelude

import Control.Monad.Eff (Eff)
import React (ReactClass, ReactElement, createElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import edgeShapesClass  :: forall props. ReactClass props
foreign import filterClass      :: forall props. ReactClass props
foreign import forceAtlas2Class :: forall props. ReactClass props
foreign import forceLinkClass   :: forall props. ReactClass props
foreign import loadGEXFClass    :: forall props. ReactClass props
foreign import loadJSONClass    :: forall props. ReactClass props
foreign import nOverlapClass    :: forall props. ReactClass props
foreign import neoCypherClass   :: forall props. ReactClass props
foreign import neoGraphItemsProducersClass :: forall props. ReactClass props
foreign import nodeShapesClass             :: forall props. ReactClass props
foreign import randomizeNodePositionsClass :: forall props. ReactClass props
foreign import relativeSizeClass           :: forall props. ReactClass props
foreign import sigmaClass :: forall props. ReactClass props
foreign import sigmaEnableSVGClass :: forall props. ReactClass props
foreign import sigmaEnableWebGLClass :: forall props. ReactClass props

neoCypher :: forall eff o. Optional o (NeoCypherOptProps eff)  => NeoCypherReqProps o -> ReactElement
neoCypher props = createElement neoCypherClass props []

loadJSON :: forall eff o. Optional o (onGraphLoaded :: Eff eff Unit) => { "path" :: String | o } -> ReactElement
loadJSON props = createElement loadJSONClass props []

loadGEXF :: forall eff o. Optional o (onGraphLoaded :: Eff eff Unit) => { "path" :: String | o } -> ReactElement
loadGEXF props = createElement loadGEXFClass props []

forceLink :: forall eff o. Optional o (ForceLinkOptProps eff)  => { | o} -> ReactElement
forceLink props = createElement forceLinkClass props []

nOverlap :: forall o. Optional o NOverlapOptProps  => { | o } -> ReactElement
nOverlap props = createElement nOverlapClass props []

randomizeNodePositions :: ReactElement
randomizeNodePositions  = createElement randomizeNodePositionsClass {} []

relativeSize :: {initialSize :: Number } -> ReactElement
relativeSize props = createElement randomizeNodePositionsClass props []

forceAtlas2 :: forall eff o. Optional o (ForceAtlas2OptProps eff)  => { | o } -> ReactElement
forceAtlas2 props = createElement forceAtlas2Class props []

sigma :: forall props eff. Optional props (SigmaProps eff) =>  { | props} -> Array ReactElement -> ReactElement
sigma = createElement sigmaClass

sigmaEnableWebGL :: ReactElement
sigmaEnableWebGL = createElement sigmaEnableWebGLClass {} []

edgeShapes :: { "default" :: EdgeShape } -> ReactElement
edgeShapes props = createElement edgeShapesClass props []

nodeShapes :: { "default" :: NodeShape } -> ReactElement
nodeShapes props = createElement nodeShapesClass props []


foreign import data SigmaNode :: Type
foreign import data SigmaEdge :: Type
foreign import data SigmaSettings :: Type

-- | Proof that row `r` is a subset of row `s`
class Optional (r :: # Type) (s :: # Type)
instance srInstance :: Union r t s => Optional r s



type NeoCypherOptProps eff =
  ( producers :: String
  , onGraphLoaded :: Eff eff Unit
  )

type NeoCypherReqProps o =
  { url :: String
  , user :: String
  , password :: String
  , query :: String
  | o
  }



type ForceLinkOptProps eff =
  ( barnesHutOptimize :: Boolean
  , barnesHutTheta :: Number
  , adjustSizes :: Boolean
  , iterationsPerRender :: Number
  , linLogMode :: Boolean
  , outboundAttractionDistribution :: Boolean
  , edgeWeightInfluence :: Number
  , scalingRatio :: Number
  , strongGravityMode :: Boolean
  , gravity :: Number
  , alignNodeSiblings :: Boolean
  , nodeSiblingsScale :: Number
  , nodeSiblingsAngleMin :: Number
  , worker :: Boolean
  , background :: Boolean
  , easing :: SigmaEasing
  , randomize :: Randomize
  , slowDown :: Number
  , timeout :: Number
  )

newtype Randomize = Randomize String

randomize :: { globally :: Randomize
, locally :: Randomize
, no :: Randomize
}
randomize =
  { globally : Randomize "globally"
  , locally : Randomize "locally"
  , no : Randomize "no"
  }

newtype SigmaEasing = SigmaEasing String

sigmaEasing :: { linear :: SigmaEasing
, quadraticIn :: SigmaEasing
, quadraticOut :: SigmaEasing
, quadraticInOut :: SigmaEasing
, cubicIn :: SigmaEasing
, cubicOut :: SigmaEasing
, cubicInOut :: SigmaEasing
}
sigmaEasing =
  { linear : SigmaEasing "linear"
  , quadraticIn : SigmaEasing "quadraticIn"
  , quadraticOut : SigmaEasing "quadraticOut"
  , quadraticInOut : SigmaEasing "quadraticInOut"
  , cubicIn : SigmaEasing "cubicIn"
  , cubicOut : SigmaEasing "cubicOut"
  , cubicInOut : SigmaEasing "cubicInOut"
  }

type ForceAtlas2OptProps eff =
  ( worker :: Boolean
  , barnesHutOptimize :: Boolean
  , barnesHutTheta :: Number
  , adjustSizes :: Boolean
  , iterationsPerRender :: Number
  , linLogMode :: Boolean
  , outboundAttractionDistribution :: Boolean
  , edgeWeightInfluence :: Number
  , scalingRatio :: Number
  , strongGravityMode :: Boolean
  , slowDown :: Number
  , gravity :: Number
  , timeout :: Number
  , fixedY  :: Boolean
  , startingIterations :: Number
  , skipHidden :: Boolean
  )


type NOverlapOptProps =
  ( nodes :: Array SigmaNode
  , nodeMargin :: Number
  , scaleNodes :: Number
  , gridSize :: Number
  , permittedExpansion :: Number
  , speed :: Number
  , maxIterations :: Number
  , easing :: SigmaEasing
  , duration :: Number
  )

type SigmaNodeEvent =
  { "data" ::
       { node :: {id :: Int, label :: String}
       , captor ::
            { clientX :: Number
            , clientY :: Number
            }
       }
  }

type SigmaEdgeEvent =
  { "data"::
       { node :: SigmaEdge
       , captor ::
            { clientX :: Number
            ,  clientY :: Number
            }
       }
  }

newtype Renderer = Renderer String

webgl :: Renderer
webgl = Renderer "webgl"

canvas :: Renderer
canvas = Renderer "canvas"

newtype Color = Color String

newtype SigmaGraphData = SigmaGraphData
  { nodes :: Array SigmaNode
  , edges :: Array SigmaEdge
  }

type SigmaNodeOptProps =
  ( x :: Number
  , y :: Number
  , size :: Number
  , color :: Color
  , label :: String
  )

type SigmaNodeReqProps o =
  { id :: String
  | o
  }

type SigmaEdgeOptProps =
  ( color :: Color
  , label :: String
  , "type" :: String
  )

type SigmaEdgeReqProps o =
  { id :: String
  , source :: String
  , target :: String
  | o
  }

sigmaNode :: forall o. Optional o SigmaNodeOptProps => SigmaNodeReqProps o -> SigmaNode
sigmaNode = unsafeCoerce

sigmaEdge :: forall o. Optional o SigmaEdgeOptProps => SigmaEdgeReqProps o -> SigmaEdge
sigmaEdge = unsafeCoerce

-- se_ex01 :: SigmaEdge
-- se_ex01 = sigmaEdge { id : "", source : "", target : "", label : ""}

-- sn_ex01 :: SigmaNode
-- sn_ex01 = sigmaNode { id : "", label : ""}

sigmaSettings :: forall o. Optional o SigmaSettingProps => { | o } -> SigmaSettings
sigmaSettings = unsafeCoerce

foreign import data SigmaStyle :: Type

type SigmaProps eff =
  ( renderer :: Renderer
  , settings :: SigmaSettings
  , style :: SigmaStyle
  , graph :: SigmaGraphData
  , onClickNode :: SigmaNodeEvent -> Unit
  , onOverNode :: SigmaNodeEvent -> Unit
  , onOutNode :: SigmaNodeEvent -> Eff eff Unit
  , onClickEdge :: SigmaEdgeEvent -> Eff eff Unit
  , onOverEdge :: SigmaEdgeEvent -> Eff eff Unit
  , onOutEdge :: SigmaEdgeEvent -> Eff eff Unit
  )

sStyle :: forall style. { | style } -> SigmaStyle
sStyle = unsafeCoerce

newtype EdgeShape = EdgeShape String

edgeShape :: { line :: EdgeShape
, arrow :: EdgeShape
, curve :: EdgeShape
, curvedArrow :: EdgeShape
, dashed :: EdgeShape
, dotted :: EdgeShape
, parallel :: EdgeShape
, tapered :: EdgeShape
}
edgeShape =
  { line : EdgeShape "line"
  , arrow : EdgeShape "arrow"
  , curve : EdgeShape "curve"
  , curvedArrow : EdgeShape "curvedArrow"
  , dashed : EdgeShape "dashed"
  , dotted : EdgeShape "dotted"
  , parallel : EdgeShape "parallel"
  , tapered : EdgeShape "tapered"
  }




newtype NodeShape = NodeShape String

nodeShape :: { def :: NodeShape
, pacman :: NodeShape
, star :: NodeShape
, equilateral :: NodeShape
, cross :: NodeShape
, diamond :: NodeShape
, circle :: NodeShape
, square :: NodeShape
}
nodeShape =
  { def : NodeShape "def"
  , pacman : NodeShape "pacman"
  , star : NodeShape "star"
  , equilateral : NodeShape "equilateral"
  , cross : NodeShape "cross"
  , diamond : NodeShape "diamond"
  , circle : NodeShape "circle"
  , square : NodeShape "square"
  }

newtype ScalingMode = ScalingMode String

scalingMode :: { inside :: ScalingMode
, outside :: ScalingMode
}
scalingMode =
  { inside : ScalingMode "inside"
  , outside : ScalingMode "outside"
  }



type SigmaSettingProps =
  ( clone :: Boolean
  , immutable :: Boolean
  , verbose :: Boolean
  , defaultNodeType :: String
  , defaultEdgeType :: String
  , defaultLabelColor :: String
  , defaultEdgeCOlor :: String
  , defaultNodeColor :: String
  , defaultLabelSize :: String
  , edgeColor :: String
  , minArrowSize :: Number
  , font :: String
  , fontStyle :: String
  , labelColor :: String
  , labelSize :: String
  , labelSizeRatio :: Number
  , labelThreshold :: Number
  , labelMaxSize :: Number
  , webglOversamplingRatio :: Number
  , borderSize :: Number
  , nodeBorderColor :: String
  , defaultNodeBorderColor :: String
  , hoverFont :: String
  , hoverFontStyle :: String
  , labelHoverShadow :: String
  , labelHoverShadowColor :: String
  , nodeHoverColor :: String
  , defaultNodeHoverColor :: String
  , labelHoverBGColor :: String
  , defaultHoverLabelBGColor :: String
  , defaultHoverLabelColor :: String
  , labelHoverColor :: String
  , defaultLabelHoverColor :: String
  , singleHover :: Boolean
  , edgeHoverColor :: String
  , defaultEdgeHoverColor :: String
  , edgeHoverSizeRatio :: Number
  , edgeHoverExtremities :: Boolean
  , drawLabels :: Boolean
  , drawEdgeLabels :: Boolean
  , drawEdges :: Boolean
  , drawNodes :: Boolean
  , batchEdgesDrawing :: Boolean
  , canvasEdgesBatchSize :: Number
  , webglEdgesBatchSize :: Number
  , hideEdgesOnMove :: Boolean
  , scalingMode :: ScalingMode
  , sideMargin :: Number
  , minEdgeSize :: Number
  , maxEdgeSize :: Number
  , minNodeSize :: Number
  , maxNodeSize :: Number
  , touchEnabled :: Boolean
  , mouseEnabled :: Boolean
  , mouseWheelEnabled :: Boolean
  , doubleClickEnabled :: Boolean
  , eventsEnabled :: Boolean
  , zoomingRatio :: Number
  , doubleClickZoomingRatio :: Number
  , zoomMin :: Number
  , zoomMax :: Number
  , mouseZoomDuration :: Number
  , doubleClickZoomDuration :: Number
  , mouseInertiaDuration :: Number
  , mouseInertiaRatio :: Number
  , touchInertiaDuration :: Number
  , touchInertiaRatio :: Number
  , doubleClickTimeout :: Number
  , doubleTapTimeout :: Number
  , dragTimeout :: Number
  , autoResize :: Boolean
  , autoRescale :: Boolean
  , enableCamera :: Boolean
  , enableHovering :: Boolean
  , enableEdgeHovering :: Boolean
  , edgeHoverPrecision :: Number
  , rescaleIgnoreSize :: Boolean
  , skipErrors :: Boolean
  , nodesPowRatio :: Number
  , edgesPowRatio :: Number
  , animationsTime :: Number
  , twNodeRendBorderSize :: Number
  , twNodeRendBorderColor :: String
  , twEdgeDefaultOpacity  :: Number
  , twSelectedColor       :: String
  , twNodesGreyOpacity    :: Number
  , twBorderGreyColor     :: String
  , twEdgeGreyColor       :: String
  )
