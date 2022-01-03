module Gargantext.Hooks.Sigmax.Types where

import DOM.Simple.Types (Element)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Show, map, ($), (&&), (==), (||), (<$>), mod, not)

import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Data.Louvain as Louvain
import Gargantext.Types as GT

newtype Graph n e = Graph { edges :: Seq.Seq {|e}, nodes :: Seq.Seq {|n} }

derive instance Generic (Graph n e) _

instance (Eq (Record n), Eq (Record e)) => Eq (Graph n e) where
  eq = genericEq

--instance Eq Graph where
--  eq (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = n1 == n2 && e1 == e2


type Renderer = { "type" :: String, container :: Element }

type NodeId = String
type EdgeId = String

type Node = (
    borderColor :: String
  , children    :: Array String
  , color       :: String
  , equilateral :: { numPoints :: Int }
  , gargType    :: GT.Mode
  , hidden      :: Boolean
  , id          :: NodeId
  , label       :: String
  , size        :: Number
  , type        :: String  -- available types: circle, cross, def, diamond, equilateral, pacman, square, star
  , x           :: Number
  , y           :: Number
  , _original   :: GET.Node
  )

type Edge = (
    color      :: String
  , confluence :: Number
  , id         :: EdgeId
  , hidden     :: Boolean
  , size       :: Number
  , source     :: NodeId
  , sourceNode :: Record Node
  , target     :: NodeId
  , targetNode :: Record Node
  , weight     :: Number
  , weightIdx  :: Int
  , _original  :: GET.Edge
  )

type NodeIds = Set.Set NodeId
type EdgeIds = Set.Set EdgeId
type EdgesMap = Map.Map String (Record Edge)
type NodesMap = Map.Map String (Record Node)

emptyEdgeIds :: EdgeIds
emptyEdgeIds = Set.empty
emptyNodeIds :: NodeIds
emptyNodeIds = Set.empty

type SGraph = Graph Node Edge

-- Diff graph structure
-- NOTE: "add" is NOT a graph. There can be edges which join nodes that are not
-- in the SigmaDiff nodes array.
type SigmaDiff =
  (
    add :: Tuple (Seq.Seq (Record Edge)) (Seq.Seq (Record Node))
  , remove :: Tuple EdgeIds NodeIds
  )

graphEdges :: SGraph -> Seq.Seq (Record Edge)
graphEdges (Graph {edges}) = edges

graphNodes :: SGraph -> Seq.Seq (Record Node)
graphNodes (Graph {nodes}) = nodes

edgesGraphMap :: SGraph -> EdgesMap
edgesGraphMap graph =
  Map.fromFoldable $ map (\e -> Tuple e.id e) $ graphEdges graph

edgesFilter :: (Record Edge -> Boolean) -> SGraph -> SGraph
edgesFilter f (Graph {edges, nodes}) = Graph { edges: Seq.filter f edges, nodes }

nodesMap :: Seq.Seq (Record Node) -> NodesMap
nodesMap nodes = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes

nodesGraphMap :: SGraph -> NodesMap
nodesGraphMap graph =
  nodesMap $ graphNodes graph

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

-- | Compute a diff between current sigma graph and whatever is set via customer controls
sigmaDiff :: EdgeIds -> NodeIds -> SGraph -> Record SigmaDiff
sigmaDiff sigmaEdges sigmaNodes g@(Graph {nodes, edges}) = {add, remove}
  where
    add = Tuple addEdges addNodes
    remove = Tuple removeEdges removeNodes

    addG = edgesFilter (\e -> not (Set.member e.id sigmaEdges)) $ nodesFilter (\n -> not (Set.member n.id sigmaNodes)) g
    addEdges = graphEdges addG
    addNodes = graphNodes addG

    removeEdges = Set.difference sigmaEdges (Set.fromFoldable $ Seq.map _.id edges)
    removeNodes = Set.difference sigmaNodes (Set.fromFoldable $ Seq.map _.id nodes)

neighbours :: SGraph -> Seq.Seq (Record Node) -> Seq.Seq (Record Node)
neighbours g nodes = Seq.fromFoldable $ Set.unions [Set.fromFoldable nodes, sources, targets]
  where
    nodeIds = Set.fromFoldable $ Seq.map _.id nodes
    selectedEdges = neighbouringEdges g nodeIds
    sources = Set.fromFoldable $ graphNodes $ nodesById g $ Set.fromFoldable $ Seq.map _.source selectedEdges
    targets = Set.fromFoldable $ graphNodes $ nodesById g $ Set.fromFoldable $ Seq.map _.target selectedEdges

neighbouringEdges :: SGraph -> NodeIds -> Seq.Seq (Record Edge)
neighbouringEdges g nodeIds = Seq.filter condition $ graphEdges g
  where
    condition {source, target} = (Set.member source nodeIds) || (Set.member target nodeIds)

eqGraph :: SGraph -> SGraph -> Boolean
eqGraph (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = (n1 == n2) && (e1 == e2)


-- | Custom state for force atlas. Basically, it can be "Running" or "Paused"
-- however when graph is loaded initially, forceAtlas is running for a couple of
-- seconds and then stops (unless the user alters this by clicking the toggle
-- button).
data ForceAtlasState = InitialRunning | InitialStopped | Running | Paused | Killed

derive instance Generic ForceAtlasState _
instance Eq ForceAtlasState where
  eq = genericEq

toggleForceAtlasState :: ForceAtlasState -> ForceAtlasState
toggleForceAtlasState InitialRunning = Paused
toggleForceAtlasState InitialStopped = InitialRunning
toggleForceAtlasState Running = Paused
toggleForceAtlasState Paused = Running
toggleForceAtlasState Killed = InitialRunning

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

-- | Return the temporary hidden state, if applicable.
edgeStateTempHide :: ShowEdgesState -> ShowEdgesState
edgeStateTempHide EHide = EHide
edgeStateTempHide _ = ETempHiddenThenShow

-- | Whether, after disabling the temp state, edges will be shown or hidden.
edgeStateWillBeHidden :: ShowEdgesState -> Boolean
edgeStateWillBeHidden EHide = true
edgeStateWillBeHidden _ = false

-- | Get rid of the temporary transition
edgeStateStabilize :: ShowEdgesState -> ShowEdgesState
edgeStateStabilize ETempHiddenThenShow = EShow
edgeStateStabilize s = s

-- | Return state in which showEdges should be depending on forceAtlasState
forceAtlasEdgeState :: ForceAtlasState -> ShowEdgesState -> ShowEdgesState
forceAtlasEdgeState InitialRunning EShow = ETempHiddenThenShow
forceAtlasEdgeState InitialRunning es = es
forceAtlasEdgeState InitialStopped es = es
forceAtlasEdgeState Running EShow = ETempHiddenThenShow
forceAtlasEdgeState Running es = es
forceAtlasEdgeState Paused ETempHiddenThenShow = EShow
forceAtlasEdgeState Paused es = es
forceAtlasEdgeState Killed ETempHiddenThenShow = EShow
forceAtlasEdgeState Killed es = es


louvainEdges :: SGraph -> Array (Record Louvain.Edge)
louvainEdges g = Seq.toUnfoldable $ Seq.map (\{source, target, weight} -> {source, target, weight}) (graphEdges g)
louvainNodes :: SGraph -> Array Louvain.Node
louvainNodes g = Seq.toUnfoldable $ Seq.map _.id (graphNodes g)

louvainGraph :: SGraph -> Louvain.LouvainCluster -> SGraph
louvainGraph g cluster = Graph {nodes: newNodes, edges: newEdges}
  where
    nodes = graphNodes g
    edges = graphEdges g

    newNodes = (nodeClusterColor cluster) <$> nodes
    nm = nodesMap newNodes
    newEdges = (edgeClusterColor cluster nm) <$> edges

edgeClusterColor cluster nm e = e { color = sourceNode.color, sourceNode = sourceNode, targetNode = targetNode }
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
