module Gargantext.Hooks.Sigmax.Types where

import DOM.Simple.Types (Element)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, map, ($), (&&), (==), (||))

newtype Graph n e = Graph { nodes :: Seq.Seq {|n}, edges :: Seq.Seq {|e} }

--derive instance eqGraph :: Eq Graph

--instance eqGraph :: Eq Graph where
--  eq (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = n1 == n2 && e1 == e2


type Renderer = { "type" :: String, container :: Element }


type Node =
  ( borderColor :: String
  , color :: String
  , hidden :: Boolean
  , id    :: String
  , label :: String
  , size  :: Number
  , type  :: String  -- available types: circle, cross, def, diamond, equilateral, pacman, square, star
  , x     :: Number
  , y     :: Number
  )

type Edge =
  ( color :: String
  , confluence :: Number
  , id :: String
  , hidden :: Boolean
  , size :: Number
  , source :: String
  , sourceNode :: Record Node
  , target :: String
  , targetNode :: Record Node
  , weight :: Number )

type SelectedNodeIds = Set.Set String
type SelectedEdgeIds = Set.Set String
type EdgesMap = Map.Map String (Record Edge)
type NodesMap = Map.Map String (Record Node)

type SGraph = Graph Node Edge

graphEdges :: SGraph -> Seq.Seq (Record Edge)
graphEdges (Graph {edges}) = edges

graphNodes :: SGraph -> Seq.Seq (Record Node)
graphNodes (Graph {nodes}) = nodes

edgesGraphMap :: SGraph -> EdgesMap
edgesGraphMap graph =
  Map.fromFoldable $ map (\e -> Tuple e.id e) $ graphEdges graph

edgesById :: SGraph -> SelectedEdgeIds -> Seq.Seq (Record Edge)
edgesById g edgeIds = Seq.filter (\e -> Set.member e.id edgeIds) $ graphEdges g

nodesMap :: Seq.Seq (Record Node) -> NodesMap
nodesMap nodes = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes

nodesGraphMap :: SGraph -> NodesMap
nodesGraphMap graph =
  nodesMap $ graphNodes graph

nodesById :: SGraph -> SelectedNodeIds -> Seq.Seq (Record Node)
nodesById g nodeIds = Seq.filter (\n -> Set.member n.id nodeIds) $ graphNodes g

neighbours :: SGraph -> Seq.Seq (Record Node) -> Seq.Seq (Record Node)
neighbours g nodes = Seq.fromFoldable $ Set.unions [Set.fromFoldable nodes, sources, targets]
  where
    nodeIds = Set.fromFoldable $ Seq.map _.id nodes
    selectedEdges = neighbouringEdges g nodeIds
    sources = Set.fromFoldable $ nodesById g $ Set.fromFoldable $ Seq.map _.source selectedEdges
    targets = Set.fromFoldable $ nodesById g $ Set.fromFoldable $ Seq.map _.target selectedEdges

neighbouringEdges :: SGraph -> SelectedNodeIds -> Seq.Seq (Record Edge)
neighbouringEdges g nodeIds = Seq.filter condition $ graphEdges g
  where
    condition {source, target} = (Set.member source nodeIds) || (Set.member target nodeIds)

eqGraph :: SGraph -> SGraph -> Boolean
eqGraph (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = (n1 == n2) && (e1 == e2)


-- | Custom state for force atlas. Basically, it can be "Running" or "Paused"
-- however when graph is loaded initially, forceAtlas is running for a couple of
-- seconds and then stops (unless the user alters this by clicking the toggle
-- button).
data ForceAtlasState = InitialRunning | Running | Paused

derive instance genericForceAtlasState :: Generic ForceAtlasState _
instance eqForceAtlasState :: Eq ForceAtlasState where
  eq = genericEq

toggleForceAtlasState :: ForceAtlasState -> ForceAtlasState
toggleForceAtlasState InitialRunning = Paused
toggleForceAtlasState Running = Paused
toggleForceAtlasState Paused = Running

-- | Custom state for show edges. Normally it is EShow or EHide (show/hide
-- | edges). However, edges are temporarily turned off when forceAtlas is
-- | running.
data ShowEdgesState = EShow | EHide | ETempHiddenThenShow

derive instance genericShowEdgesState :: Generic ShowEdgesState _
instance eqShowEdgesState :: Eq ShowEdgesState where
  eq = genericEq
instance showShowEdgesState :: Show ShowEdgesState where
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
forceAtlasEdgeState Running EShow = ETempHiddenThenShow
forceAtlasEdgeState Running es = es
forceAtlasEdgeState Paused ETempHiddenThenShow = EShow
forceAtlasEdgeState Paused es = es
