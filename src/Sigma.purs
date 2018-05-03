module Sigma where

import React (ReactClass)
import React.DOM.Props (Props, unsafeMkProps)

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

settings:: String -> Props -- Sigma$Settings,
settings = unsafeMkProps "settings"

renderer:: String -> Props -- "webgl" | "canvas" | "svg",
renderer = unsafeMkProps "renderer"

style:: String -> Props -- Object,
style = unsafeMkProps "style"

children:: String -> Props -- mixed,
children = unsafeMkProps "children"

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


_default ::  String -> Props --Sigma$Edge$Shapes,
_default =  unsafeMkProps "default"

sigma:: String -> Props --Sigma
sigma = unsafeMkProps "sigma"


-- type Sigma$Edge$Shapes = "line" | "arrow" | "curve" | "curvedArrow" | "dashed" | "dotted" | "parallel" | "tapered"

-- type Sigma$Node$Shapes = "def" | "pacman" | "star" | "equilateral" | "cross" | "diamond" | "circle" | "square"



path  ::  String -> Props --  string,
path = unsafeMkProps "path"

onGraphLoaded  ::  String -> Props -- ?: () => void,
onGraphLoaded = unsafeMkProps "onGraphLoaded"

children  ::  String -> Props --mixed,
children = unsafeMkProps "children"

-- sigma ::  String -> Props --Sigma
-- sigma = unsafeMkProps "sigma"


nodesBy ::  String -> Props -- ?: Nodes$Filter,
nodesBy = unsafeMkProps "nodesBy"

neighborsOfString :: String -> Props -- ?: string,
neighborsOfString = unsafeMkProps "neighborsOfString"


--forceatlas2 props
