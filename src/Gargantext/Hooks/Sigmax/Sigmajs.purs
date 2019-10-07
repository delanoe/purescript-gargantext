module Gargantext.Hooks.Sigmax.Sigmajs where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React (ReactRef, SyntheticEventHandler)
import React.SyntheticEvent (SyntheticMouseEvent)
import Record.Unsafe (unsafeGet)
import Unsafe.Coerce (unsafeCoerce)
import Gargantext.Types (class Optional)

foreign import data SigmaNode :: Type

foreign import data SigmaEdge :: Type

foreign import data SigmaSettings :: Type

type SigmaNodeEvent
  = { "data" ::
      { node :: { id :: Int, label :: String }
      , captor ::
        { clientX :: Number
        , clientY :: Number
        }
      }
    }

type SigmaEdgeEvent
  = { "data" ::
      { node :: SigmaEdge
      , captor ::
        { clientX :: Number
        , clientY :: Number
        }
      }
    }

newtype SigmaGraphData
  = SigmaGraphData
  { nodes :: Array SigmaNode
  , edges :: Array SigmaEdge
  }

type SigmaNodeOptProps
  = ( x :: Number
    , y :: Number
    , size :: Number
    , color :: String
    , label :: String
    )

type SigmaNodeReqProps o
  = { id :: String
    | o
    }

type SigmaEdgeOptProps
  = ( color :: String
    , label :: String
    , "type" :: String
    )

type SigmaEdgeReqProps o
  = { id :: String
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

type CameraProps
  = ( x :: Number
    , y :: Number
    , ratio :: Number
    , angle :: Number
    )

foreign import data SigmaInstance' :: #Type

foreign import data CameraInstance' :: #Type

type SigmaInstance
  = { | SigmaInstance' }

type CameraInstance
  = { | CameraInstance' }

foreign import sigmaOnMouseMove :: { cursorSize :: Number } -> SyntheticMouseEvent -> Effect Unit

cameras :: SigmaInstance -> Array CameraInstance
cameras = unsafeGet "cameras"

getCameraProps :: CameraInstance -> { | CameraProps }
getCameraProps = unsafeCoerce

foreign import goToImpl :: forall o. CameraInstance -> EffectFn1 { | o } CameraInstance

goTo :: forall o. Optional o CameraProps => CameraInstance -> { | o } -> Effect CameraInstance
goTo cam = runEffectFn1 (goToImpl cam)

foreign import pauseForceAtlas2 :: Effect Unit

type SigmaProps
  = ( renderer :: String
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

newtype EdgeShape
  = EdgeShape String

edgeShape ::
  { line :: EdgeShape
  , arrow :: EdgeShape
  , curve :: EdgeShape
  , curvedArrow :: EdgeShape
  , dashed :: EdgeShape
  , dotted :: EdgeShape
  , parallel :: EdgeShape
  , tapered :: EdgeShape
  }
edgeShape =
  { line: EdgeShape "line"
  , arrow: EdgeShape "arrow"
  , curve: EdgeShape "curve"
  , curvedArrow: EdgeShape "curvedArrow"
  , dashed: EdgeShape "dashed"
  , dotted: EdgeShape "dotted"
  , parallel: EdgeShape "parallel"
  , tapered: EdgeShape "tapered"
  }

newtype NodeShape
  = NodeShape String

nodeShape ::
  { def :: NodeShape
  , pacman :: NodeShape
  , star :: NodeShape
  , equilateral :: NodeShape
  , cross :: NodeShape
  , diamond :: NodeShape
  , circle :: NodeShape
  , square :: NodeShape
  }
nodeShape =
  { def: NodeShape "def"
  , pacman: NodeShape "pacman"
  , star: NodeShape "star"
  , equilateral: NodeShape "equilateral"
  , cross: NodeShape "cross"
  , diamond: NodeShape "diamond"
  , circle: NodeShape "circle"
  , square: NodeShape "square"
  }
