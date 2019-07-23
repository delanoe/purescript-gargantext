module Gargantext.Hooks.Sigmax.Sigmajs where

import Prelude

import Data.Nullable (Nullable)
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1)
import React (Children, ReactClass, ReactElement, ReactRef, SyntheticEventHandler, createElement, unsafeCreateElement)
import React.SyntheticEvent (SyntheticMouseEvent)
import Record.Unsafe (unsafeGet)
import Unsafe.Coerce (unsafeCoerce)
import Gargantext.Types (class Optional)

foreign import edgeShapesClass  :: forall props. ReactClass props
foreign import filterClass      :: forall props. ReactClass props
foreign import forceAtlas2Class :: forall props. ReactClass props
foreign import forceLinkClass   :: forall props. ReactClass props
foreign import loadGEXFClass    :: forall props. ReactClass props
foreign import loadJSONClass    :: forall props. ReactClass props
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

forceLink :: forall o. Optional o ForceLinkOptProps  => { | o} -> ReactElement
forceLink props = unsafeCreateElement forceLinkClass props []

nOverlap :: forall o. Optional o NOverlapOptProps  => { | o } -> ReactElement
nOverlap props = unsafeCreateElement nOverlapClass (unsafeCoerce props) []

randomizeNodePositions :: ReactElement
randomizeNodePositions  = createElement randomizeNodePositionsClass {} []

relativeSize :: {initialSize :: Number } -> ReactElement
relativeSize props = unsafeCreateElement randomizeNodePositionsClass (unsafeCoerce props) []

foreign import data SigmaNode :: Type
foreign import data SigmaEdge :: Type
foreign import data SigmaSettings :: Type

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

randomize ::
  { globally :: Randomize
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
            , clientY :: Number
            }
       }
  }

newtype Renderer = Renderer String

webgl :: Renderer
webgl = Renderer "webgl"

canvas :: Renderer
canvas = Renderer "canvas"

newtype SigmaGraphData = SigmaGraphData
  { nodes :: Array SigmaNode
  , edges :: Array SigmaEdge
  }

type SigmaNodeOptProps =
  ( x :: Number
  , y :: Number
  , size :: Number
  , color :: String
  , label :: String
  )

type SigmaNodeReqProps o =
  { id :: String
  | o
  }

type SigmaEdgeOptProps =
  ( color :: String
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

foreign import data SigmaStyle :: Type

type CameraProps =
  ( x :: Number
  , y :: Number
  , ratio :: Number
  , angle :: Number
  )

foreign import data SigmaInstance' :: # Type
foreign import data CameraInstance' :: # Type
type SigmaInstance = { | SigmaInstance' }
type CameraInstance = { | CameraInstance' }
foreign import setSigmaRef :: EffectFn1 (Nullable ReactRef) Unit
foreign import getSigmaRef :: Effect SigmaInstance
foreign import sigmaOnMouseMove :: {cursorSize :: Number} -> SyntheticMouseEvent -> Effect Unit
cameras :: SigmaInstance -> Array CameraInstance
cameras = unsafeGet "cameras"

getCameraProps :: CameraInstance -> { | CameraProps }
getCameraProps = unsafeCoerce

foreign import goToImpl :: forall o. CameraInstance -> EffectFn1 { | o } CameraInstance

goTo :: forall o. Optional o CameraProps => CameraInstance -> { | o } -> Effect CameraInstance
goTo cam = runEffectFn1 (goToImpl cam)

foreign import pauseForceAtlas2 :: Effect Unit

type SigmaProps =
  ( renderer :: Renderer
  , settings :: SigmaSettings
  , style :: SigmaStyle
  , graph :: SigmaGraphData
  , ref :: SyntheticEventHandler (Nullable ReactRef)
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

