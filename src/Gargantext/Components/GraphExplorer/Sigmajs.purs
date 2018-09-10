module Gargantext.Components.GraphExplorer.Sigmajs where

import Prelude

import Effect (Effect)
import Prim.Row (class Union)
import React (Children, ReactClass, ReactElement, createElement, unsafeCreateElement)
import Unsafe.Coerce (unsafeCoerce)

foreign import edgeShapesClass  :: forall props. ReactClass props
foreign import filterClass      :: forall props. ReactClass props
foreign import forceAtlas2Class :: forall props. ReactClass props
foreign import forceLinkClass   :: forall props. ReactClass props
foreign import loadGEXFClass    :: forall props. ReactClass props
foreign import loadJSONClass    :: forall props. ReactClass props
foreign import loadGraphClass   :: forall props. ReactClass props
foreign import nOverlapClass    :: ReactClass {children :: Children}
foreign import neoCypherClass   :: ReactClass {children :: Children}
foreign import neoGraphItemsProducersClass :: forall props. ReactClass props
foreign import nodeShapesClass             :: ReactClass {children :: Children}
foreign import randomizeNodePositionsClass :: ReactClass {children :: Children}
foreign import relativeSizeClass           :: forall props. ReactClass props
foreign import sigmaClass :: ReactClass {children :: Children}
foreign import sigmaEnableSVGClass :: forall props. ReactClass props
foreign import sigmaEnableWebGLClass :: ReactClass {children :: Children}

neoCypher :: forall o. Optional o NeoCypherOptProps  => NeoCypherReqProps o -> ReactElement
neoCypher props = unsafeCreateElement neoCypherClass (unsafeCoerce props) []

loadJSON :: forall o. Optional o (onGraphLoaded :: Effect Unit) => { "path" :: String | o } -> ReactElement
loadJSON props = unsafeCreateElement loadJSONClass props []

loadGEXF :: forall o. Optional o (onGraphLoaded :: Effect Unit) => { "path" :: String | o } -> ReactElement
loadGEXF props = unsafeCreateElement loadGEXFClass props []

loadGraph :: forall o. Optional o (onGraphLoaded :: Effect Unit) => { graph :: SigmaGraphData | o } -> ReactElement
loadGraph props = unsafeCreateElement loadGraphClass props []

forceLink :: forall o. Optional o ForceLinkOptProps  => { | o} -> ReactElement
forceLink props = unsafeCreateElement forceLinkClass props []

nOverlap :: forall o. Optional o NOverlapOptProps  => { | o } -> ReactElement
nOverlap props = unsafeCreateElement nOverlapClass (unsafeCoerce props) []

randomizeNodePositions :: ReactElement
randomizeNodePositions  = createElement randomizeNodePositionsClass {} []

relativeSize :: {initialSize :: Number } -> ReactElement
relativeSize props = unsafeCreateElement randomizeNodePositionsClass (unsafeCoerce props) []

forceAtlas2 :: forall o. Optional o ForceAtlas2OptProps  => { | o } -> ReactElement
forceAtlas2 props = unsafeCreateElement forceAtlas2Class props []

sigma :: forall props. Optional props SigmaProps =>  { | props} -> Array ReactElement -> ReactElement
sigma props children = unsafeCreateElement sigmaClass (unsafeCoerce props) children

sigmaEnableWebGL :: ReactElement
sigmaEnableWebGL = createElement sigmaEnableWebGLClass {} []

edgeShapes :: { "default" :: EdgeShape } -> ReactElement
edgeShapes props = unsafeCreateElement edgeShapesClass props []

nodeShapes :: { "default" :: NodeShape } -> ReactElement
nodeShapes props = unsafeCreateElement nodeShapesClass (unsafeCoerce props) []


foreign import data SigmaNode :: Type
foreign import data SigmaEdge :: Type
foreign import data SigmaSettings :: Type

-- | Proof that row `r` is a subset of row `s`
class Optional (r :: # Type) (s :: # Type)
instance srInstance :: Union r t s => Optional r s



type NeoCypherOptProps =
  ( producers :: String
  , onGraphLoaded :: Effect Unit
  )

type NeoCypherReqProps o =
  { url :: String
  , user :: String
  , password :: String
  , query :: String
  | o
  }



type ForceLinkOptProps =
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

type ForceAtlas2OptProps =
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

instance semigroupSigmaGraphData :: Semigroup SigmaGraphData where
  append (SigmaGraphData x) (SigmaGraphData y) = SigmaGraphData
    { nodes: x.nodes <> y.nodes
    , edges: x.edges <> y.edges
    }

instance monoidSigmaGraphData :: Monoid SigmaGraphData where
  mempty = SigmaGraphData { nodes: [], edges: [] }

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

type SigmaProps =
  ( renderer :: Renderer
  , settings :: SigmaSettings
  , style :: SigmaStyle
  , graph :: SigmaGraphData
  , onClickNode :: SigmaNodeEvent -> Unit
  , onOverNode :: SigmaNodeEvent -> Unit
  , onOutNode :: SigmaNodeEvent -> Effect Unit
  , onClickEdge :: SigmaEdgeEvent -> Effect Unit
  , onOverEdge :: SigmaEdgeEvent -> Effect Unit
  , onOutEdge :: SigmaEdgeEvent -> Effect Unit
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
