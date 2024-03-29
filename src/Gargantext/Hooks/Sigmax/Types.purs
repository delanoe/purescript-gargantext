module Gargantext.Hooks.Sigmax.Types where

import DOM.Simple.Types (Element)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Hashable (class Hashable, hash)
import Data.Show.Generic (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Show, map, ($), (&&), (==), (||), (<$>), (<), mod, not, pure, (<=))
import Record.Unsafe (unsafeGet, unsafeSet)

import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.GraphExplorer.GraphTypes as GEGT
import Gargantext.Data.Louvain as Louvain
import Gargantext.Types as GT
import Gargantext.Utils.Range as Range

newtype Graph n e = Graph { edges :: Seq.Seq {|e}, nodes :: Seq.Seq {|n} }

derive instance Generic (Graph n e) _

instance ( Eq (Record n)
         , Eq (Record e)
         , GT.Optional HashableNodeFields n
         , GT.Optional HashableEdgeFields e) => Eq (Graph n e) where
  --eq = genericEq
  eq g1@(Graph { edges: e1, nodes: n1 }) g2@(Graph { edges: e2, nodes: n2 }) =
    -- (Seq.length e1 == Seq.length e2) && (Seq.length n1 == Seq.length n2)
    compareGraphEdges g1 g2 &&
    compareGraphNodes g1 g2


compareGraphNodes :: forall n e. GT.Optional HashableNodeFields n => Graph n e -> Graph n e -> Boolean
compareGraphNodes (Graph { nodes: n1 }) (Graph { nodes: n2 }) =
  (Set.fromFoldable $ Seq.map hashNode n1) == (Set.fromFoldable $ Seq.map hashNode n2)

compareGraphEdges :: forall n e. GT.Optional HashableEdgeFields e => Graph n e -> Graph n e -> Boolean
compareGraphEdges (Graph { edges: e1 }) (Graph { edges: e2 }) =
  (Set.fromFoldable $ Seq.map hashEdge e1) == (Set.fromFoldable $ Seq.map hashEdge e2)

type Renderer = { "type" :: String, container :: Element }

type NodeId = String
type EdgeId = String

type Label = String
type Color = String

type Node = (
    borderColor :: Color
  , children    :: Array String
  , color       :: Color
  , community   :: Int  -- this is filled in by the communities-louvain graphology plugin
  , equilateral :: { numPoints :: Int }
  , gargType    :: GT.Mode
  , hidden      :: Boolean
  , highlighted :: Boolean
  , id          :: NodeId
  , label       :: Label
  , size        :: Number
  , type        :: String  -- available types: circle, cross, def, diamond, equilateral, pacman, square, star
  , x           :: Number
  , y           :: Number
  , _original   :: GEGT.Node
  )

type Edge = (
    color            :: Color
  , confluence       :: Number
  , id               :: EdgeId
  , hidden           :: Boolean
  , size             :: Number
  , source           :: NodeId
  , sourceNode       :: Record Node
  , target           :: NodeId
  , targetNode       :: Record Node
  , weight           :: Number
  , weightIdx        :: Int
  , _original        :: GEGT.Edge
  )

type NodeIds = Set.Set NodeId
type EdgeIds = Set.Set EdgeId
type EdgesMap = Map.Map String (Record Edge)
type NodesMap = Map.Map String (Record Node)

type HashableNodeFields =
  ( id          :: NodeId
  , borderColor :: Color
  , color       :: Color
  , equilateral :: { numPoints :: Int }
  , hidden      :: Boolean
  , highlighted :: Boolean
  , type        :: String )

hashNode :: forall n. GT.Optional HashableNodeFields n => {|n} -> Int
hashNode n = hash rec
  where
    rec = { id          : unsafeGet "id" n
          , borderColor : unsafeGet "borderColor" n
          , color       : unsafeGet "color" n
          , equilateral : unsafeGet "equilateral" n
          , hidden      : unsafeGet "hidden" n
          , highlighted : unsafeGet "highlighted" n
          , type        : unsafeGet "type" n } :: Record HashableNodeFields

-- | When comparing nodes, we don't want to compare all fields. Only
-- | some are relevant (when updating sigma graph).
-- NOTE For some reason, `Graphology.updateNode` throws error if `type` is set
compareNodes :: forall n. GT.Optional HashableNodeFields n => {|n} -> {|n} -> Boolean
compareNodes n1 n2 = hashNode n1 == hashNode n2

-- TODO For edges, see `Sigmax.updateEdges` (`color` and `hidden`)
type HashableEdgeFields =
  ( id     :: NodeId
  , source :: NodeId
  , target :: NodeId
  , hidden :: Boolean )

hashEdge :: forall e. GT.Optional HashableEdgeFields e => {|e} -> Int
hashEdge e = hash rec
  where
    rec = { id     : unsafeGet "id" e
          , source : unsafeGet "source" e
          , target : unsafeGet "target" e
          , hidden : unsafeGet "hidden" e } :: Record HashableEdgeFields



emptyEdgeIds :: EdgeIds
emptyEdgeIds = Set.empty
emptyNodeIds :: NodeIds
emptyNodeIds = Set.empty

type SGraph = Graph Node Edge

type NodeWithColor =
  ( color :: String
  , id    :: NodeId )

-- | Return a graph where node colors are taken from the first one and
-- | the rest is taken from second graph.
updateColors :: forall n e. GT.Optional NodeWithColor n => Map.Map NodeId Color -> Graph n e -> Graph n e
updateColors colorMap (Graph { nodes, edges }) = Graph { nodes: Seq.map updateColor nodes, edges }
  where
    updateColor n = case Map.lookup (unsafeGet "id" n) colorMap of
      Nothing -> n
      Just c  -> unsafeSet "color" c n

-- Diff graph structure
-- NOTE: "add" is NOT a graph. There can be edges which join nodes that are not
-- in the SigmaDiff nodes array.
type SigmaDiff =
  (
    add :: Tuple (Seq.Seq (Record Edge)) (Seq.Seq (Record Node))
  , remove :: Tuple EdgeIds NodeIds
  , update :: Tuple (Seq.Seq (Record Edge)) (Seq.Seq (Record Node))
  )

graphEdges :: SGraph -> Seq.Seq (Record Edge)
graphEdges (Graph {edges}) = edges

graphNodes :: SGraph -> Seq.Seq (Record Node)
graphNodes (Graph {nodes}) = nodes

idMap :: forall r t. Traversable t
      => t { id :: String | r } -> Map.Map String { id :: String | r }
idMap xs = Map.fromFoldable $ (\x@{ id } -> Tuple id x) <$> xs

edgesGraphMap :: SGraph -> EdgesMap
edgesGraphMap graph = idMap $ graphEdges graph

edgesFilter :: (Record Edge -> Boolean) -> SGraph -> SGraph
edgesFilter f (Graph {edges, nodes}) = Graph { edges: Seq.filter f edges, nodes }

nodesMap :: Seq.Seq (Record Node) -> NodesMap
nodesMap = idMap

nodesGraphMap :: SGraph -> NodesMap
nodesGraphMap graph = idMap $ graphNodes graph

nodesFilter :: (Record Node -> Boolean) -> SGraph -> SGraph
nodesFilter f (Graph {edges, nodes}) = Graph { edges, nodes: Seq.filter f nodes }

nodesById :: SGraph -> NodeIds -> SGraph
nodesById g nodeIds = nodesFilter (\n -> Set.member n.id nodeIds) g

-- | "Subtract" second graph from first one (only node/edge id's are compared, not other props)
sub :: SGraph -> SGraph -> SGraph
sub graph (Graph {nodes, edges}) = newGraph
  where
    edgeIds = Set.fromFoldable $ Seq.map _.id edges
    nodeIds = Set.fromFoldable $ Seq.map _.id nodes
    edgeFilterFunc e = (not $ Set.member e.id edgeIds)
                    && (not $ Set.member e.source nodeIds)
                    && (not $ Set.member e.target nodeIds)
    filteredEdges = edgesFilter edgeFilterFunc graph
    newGraph = nodesFilter (\n -> not (Set.member n.id nodeIds)) filteredEdges

neighbors :: SGraph -> Seq.Seq (Record Node) -> Seq.Seq (Record Node)
neighbors g nodes = Seq.fromFoldable $ Set.unions [if Set.size sources <= 1 then targets else sources]
  where
    nodeIds = Set.fromFoldable $ Seq.map _.id nodes
    selectedEdges = neighboringEdges g nodeIds
    sources = Set.fromFoldable $ graphNodes $ nodesById g $ Set.fromFoldable $ Seq.map _.source selectedEdges
    targets = Set.fromFoldable $ graphNodes $ nodesById g $ Set.fromFoldable $ Seq.map _.target selectedEdges

neighboringEdges :: SGraph -> NodeIds -> Seq.Seq (Record Edge)
neighboringEdges g nodeIds = Seq.filter condition $ graphEdges g
  where
    condition {source, target} = (Set.member source nodeIds) || (Set.member target nodeIds)

eqGraph :: SGraph -> SGraph -> Boolean
eqGraph (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = (n1 == n2) && (e1 == e2)


-- | Custom state for force atlas. Basically, it can be "Running" or "Paused"
-- however when graph is loaded initially, forceAtlas is running for a couple of
-- seconds and then stops (unless the user alters this by clicking the toggle
-- button).
data ForceAtlasState = {- InitialLoading | -} InitialRunning | InitialStopped | Running | Paused | Killed
derive instance Generic ForceAtlasState _
instance Eq ForceAtlasState where
  eq = genericEq

toggleForceAtlasState :: ForceAtlasState -> ForceAtlasState
-- toggleForceAtlasState InitialLoading = InitialRunning
toggleForceAtlasState InitialRunning = Paused
toggleForceAtlasState InitialStopped = InitialRunning
toggleForceAtlasState Running = Paused
toggleForceAtlasState Paused = Running
toggleForceAtlasState Killed = InitialRunning


forceAtlasComponentStatus :: ForceAtlasState -> ComponentStatus
-- forceAtlasComponentStatus InitialLoading = Disabled
forceAtlasComponentStatus InitialRunning = Disabled
forceAtlasComponentStatus InitialStopped = Enabled
forceAtlasComponentStatus Running = Disabled
forceAtlasComponentStatus Paused = Enabled
forceAtlasComponentStatus Killed = Enabled

data NoverlapState = NoverlapPaused | NoverlapRunning
derive instance Generic NoverlapState _
instance Eq NoverlapState where
  eq = genericEq

toggleNoverlapState :: NoverlapState -> NoverlapState
toggleNoverlapState NoverlapRunning = NoverlapPaused
toggleNoverlapState NoverlapPaused = NoverlapRunning


-- | Custom state for show edges. Normally it is EShow or EHide (show/hide
-- | edges). However, edges are temporarily turned off when forceAtlas is
-- | running.
-- | NOTE ETempHiddenThenShow state is a hack for force atlas
-- | flickering. Ideally it should be removed from here.
data ShowEdgesState = EShow | EHide | ETempHiddenThenShow
derive instance Generic ShowEdgesState _
instance Eq ShowEdgesState where
  eq = genericEq
instance Show ShowEdgesState where
  show = genericShow
instance Hashable ShowEdgesState where
  hash EShow = 0
  hash EHide = 1
  hash ETempHiddenThenShow = 2

-- | Whether the edges are hidden now (temp or "stable").
edgeStateHidden :: ShowEdgesState -> Boolean
edgeStateHidden EHide = true
edgeStateHidden ETempHiddenThenShow = true
edgeStateHidden _ = false

-- | Switch from hidden to shown, handling the temp state as well.
toggleShowEdgesState :: ShowEdgesState -> ShowEdgesState
toggleShowEdgesState s =
  if edgeStateHidden s then
    EShow
  else
    EHide

-- | Return state in which showEdges should be depending on forceAtlasState
forceAtlasEdgeState :: ForceAtlasState -> ShowEdgesState -> ShowEdgesState
-- forceAtlasEdgeState InitialLoading _ = ETempHiddenThenShow
forceAtlasEdgeState InitialRunning EShow = ETempHiddenThenShow
forceAtlasEdgeState InitialRunning es = es
forceAtlasEdgeState InitialStopped es = es
forceAtlasEdgeState Running EShow = ETempHiddenThenShow
forceAtlasEdgeState Running es = es
forceAtlasEdgeState Paused ETempHiddenThenShow = EShow
forceAtlasEdgeState Paused es = es
forceAtlasEdgeState Killed ETempHiddenThenShow = EShow
forceAtlasEdgeState Killed es = es


-- Similar situation for labels: hide them when force atlas is
-- running, to prevent flickering.
-- However, for labels this is simpler because we don't have a toggle
-- button.

-- | Return state in which labels should be depending on forceAtlasState
forceAtlasLabelState :: ForceAtlasState -> Boolean
forceAtlasLabelState InitialRunning = false
forceAtlasLabelState Running        = false
forceAtlasLabelState _              = true



louvainEdges :: SGraph -> Array (Record Louvain.Edge)
louvainEdges g = Seq.toUnfoldable $ Seq.map (\{source, target, weight} -> {source, target, weight}) (graphEdges g)
louvainNodes :: SGraph -> Array Louvain.Node
louvainNodes g = Seq.toUnfoldable $ Seq.map _.id (graphNodes g)

louvainGraph :: SGraph -> Louvain.LouvainCluster -> SGraph
louvainGraph g cluster = Graph {nodes: newNodes, edges: graphEdges g}
  where
    newNodes = (nodeClusterColor cluster) <$> (graphNodes g)
    nm = nodesMap newNodes
    newEdges = (edgeClusterColor cluster nm) <$> (graphEdges g)

--edgeClusterColor _cluster nm e = e { color = sourceNode.color, sourceNode = sourceNode, targetNode = targetNode }
edgeClusterColor _cluster nm e = e { color = sourceNode.color }
  where
    sourceNode = case Map.lookup e.source nm of
      Just sn -> sn
      Nothing -> e.sourceNode
    targetNode = case Map.lookup e.target nm of
      Just tn -> tn
      Nothing -> e.targetNode

nodeClusterColor cluster n = n { color = newColor }
  where
    newColor = case Map.lookup n.id cluster of
      Nothing -> n.color
      Just c  -> do
        let idx = c `mod` (A.length defaultPalette)
        unsafePartial $ fromJust $ defaultPalette A.!! idx

defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff"
                 ,"#b399df","#ffdfed","#33c8f3","#739e9a"
                 ,"#caeca3","#f6f7e5","#f9bcca","#ccb069"
                 ,"#c9ffde","#c58683","#6c9eb0","#ffd3cf"
                 ,"#ccffc7","#52a1b0","#d2ecff","#99fffe"
                 ,"#9295ae","#5ea38b","#fff0b3","#d99e68"
                 ]


type EdgeVisibilityProps =
  ( edgeConfluence :: Range.NumberRange
  , edgeWeight     :: Range.NumberRange
  , showEdges      :: ShowEdgesState )

setEdgeVisibility :: Record EdgeVisibilityProps -> Record Edge -> Record Edge
setEdgeVisibility { edgeConfluence, edgeWeight, showEdges } e@{ confluence, weight } = e { hidden = hidden }
  where
    hidden = (edgeStateHidden showEdges)
             || (not $ Range.within edgeConfluence confluence)
             || (not $ Range.within edgeWeight weight)
