module Sigma where

import React (ReactClass, ReactElement, createElement)
import React.DOM.Props (Props, unsafeFromPropsArray, unsafeMkProps)

foreign import sigmaClass :: forall props. ReactClass props
foreign import edgeShapesClass :: forall props. ReactClass props
foreign import nodeShapesClass :: forall props. ReactClass props
foreign import loadJSONClass :: forall props. ReactClass props
foreign import loadGEXFClass :: forall props. ReactClass props
foreign import filterClass :: forall props. ReactClass props
foreign import forceAtlas2Class :: forall props. ReactClass props
foreign import relativeSizeClass :: forall props. ReactClass props
foreign import nOverlapClass :: forall props. ReactClass props
foreign import neoCypherClass :: forall props. ReactClass props
foreign import randomizeNodePositionsClass :: forall props. ReactClass props
  --sigma props

sigmaC :: Array Props -> Array ReactElement -> ReactElement
sigmaC props = createElement sigmaClass (unsafeFromPropsArray props)

loadJSON :: Array Props -> ReactElement
loadJSON props = createElement loadJSONClass (unsafeFromPropsArray props) []

settings:: String -> Props -- Sigma$Settings,
settings = unsafeMkProps "settings"

renderer:: String -> Props -- "webgl" | "canvas" | "svg",
renderer = unsafeMkProps "renderer"

style:: forall a. a -> Props -- Object,
style = unsafeMkProps "style"

_children:: String -> Props -- mixed,
_children = unsafeMkProps "children"

graph:: String -> Props -- Sigma$Graph$Data,
graph = unsafeMkProps "graph"

onSigmaException:: String -> Props -- (e: Error) => void,
onSigmaException = unsafeMkProps "onSigmaException"

onClickNode:: String -> Props --  (e: Sigma$Event) => void,
onClickNode = unsafeMkProps "onClickNode"

onClickEdge:: String -> Props -- (e: Sigma$Event) => void,
onClickEdge = unsafeMkProps "onClickEdge"

onOverNode:: String -> Props -- (e: Sigma$Event) => void,
onOverNode = unsafeMkProps "onOverNode"

onOutNode:: String -> Props -- (e: Sigma$Event) => void,
onOutNode = unsafeMkProps "onOutNode"

onOverEdge :: String -> Props -- (e: Sigma$Event) => void, // TODO: onOverEdge does not work?
onOverEdge = unsafeMkProps "onOverEdge"

onOutEdge:: String -> Props -- (e: Sigma$Event) => void,
onOutEdge = unsafeMkProps "onOutEdge"

onClickStage:: String -> Props -- (e: Sigma$Event) => void,
onClickStage = unsafeMkProps "onClickStage"

--edge shape props

_default ::  String -> Props --Sigma$Edge$Shapes,
_default =  unsafeMkProps "default"

sigma:: String -> Props --Sigma
sigma = unsafeMkProps "sigma"


-- type Sigma$Edge$Shapes = "line" | "arrow" | "curve" | "curvedArrow" | "dashed" | "dotted" | "parallel" | "tapered"

-- type Sigma$Node$Shapes = "def" | "pacman" | "star" | "equilateral" | "cross" | "diamond" | "circle" | "square"

---loadgex props

path :: String -> Props --  string,
path = unsafeMkProps "path"

onGraphLoaded  ::  String -> Props -- ?: () => void,
onGraphLoaded = unsafeMkProps "onGraphLoaded"

children  ::  String -> Props --mixed,
children = unsafeMkProps "children"

-- sigma ::  String -> Props --Sigma
-- sigma = unsafeMkProps "sigma"

 --filter props

nodesBy ::  String -> Props -- ?: Nodes$Filter,
nodesBy = unsafeMkProps "nodesBy"

neighborsOfString :: String -> Props -- ?: string,
neighborsOfString = unsafeMkProps "neighborsOfString"


--forceatlas2 props

running :: Boolean -> Props
running = unsafeMkProps "running"

timer :: Number -> Props   -- ?: number,
timer = unsafeMkProps "timer"

drawEdges :: Boolean -> Props
drawEdges = unsafeMkProps "drawEdges"

--relativesize props

initialSize :: Number -> Props
initialSize = unsafeMkProps "initialSize"

sigmar ::  String -> Props        ---sigma r for relativesize
sigmar = unsafeMkProps "sigma"



----noverlap props



nodes::  String -> Props     --- ?: Array<Sigma$Node>
nodes = unsafeMkProps "nodes"

nodeMargin::  Number -> Props   -- ?: number
nodeMargin = unsafeMkProps "nodeMargin"

scaleNodes::  Number -> Props   -- ?: number
scaleNodes = unsafeMkProps "scaleNodes"

gridSize::  Number -> Props     -- ?: number
gridSize = unsafeMkProps "gridSize"

permittedExpansion::  Number -> Props  -- ?: number
permittedExpansion = unsafeMkProps "permittedExpansion"

speed::  Number -> Props    --- ?: number
speed = unsafeMkProps "speed"

maxIterations::  Number -> Props  -- ?: number
maxIterations = unsafeMkProps "maxIterations"

easing ::  String -> Props    --- ?: Sigma$Easing
easing = unsafeMkProps "easing"

duration::  Number -> Props   -- ?: number
duration = unsafeMkProps "duration"

sigman::  String -> Props   --- ?: sigma
sigman = unsafeMkProps "sigma"



----neocypher props
url::  String -> Props   ---- : string,
url = unsafeMkProps "url"

user ::  String -> Props  -- : string,
user = unsafeMkProps "user"

password ::  String -> Props  -- : string,
password = unsafeMkProps "password"

query ::  String -> Props   -- : string,
query = unsafeMkProps "query"

producers   ::  String -> Props   --- : ProducersInterface,
producers = unsafeMkProps "producers"


onGraphLoaded_n    ::  String -> Props    -- ?: () => void,
onGraphLoaded_n = unsafeMkProps "onGraphLoaded"

children_n  ::  String -> Props    -- ?: mixed,
children_n = unsafeMkProps "children"

sigmac ::  String -> Props    ---?: sigma
sigmac = unsafeMkProps "sigmac"



----randomize node position props


children_R  ::  String -> Props    -- ?: mixed,
children_R = unsafeMkProps "children"



sigmaR ::  String -> Props    ---?: sigma
sigmaR = unsafeMkProps "sigmac"
