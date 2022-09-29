module Gargantext.Hooks.Sigmax.Graphology where

-- FFI for graphology: https://graphology.github.io/

-- serialized graph: https://graphology.github.io/serialization#format
-- to use with: Graph.from(data)

import Prelude

import Data.Array as A
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import FFI.Simple ((..), (...), (.=))
import Gargantext.Hooks.Sigmax.Types as Types
import Record as Record

-- | Type representing `graphology.graph`
foreign import data Graph :: Type

foreign import _newGraph :: EffectFn1 Unit Graph
foreign import _addNode :: EffectFn3 Graph String (Record Types.Node) String
foreign import _updateNode :: EffectFn3 Graph String (Record Types.Node -> Record Types.Node) Unit
foreign import _addEdge :: EffectFn4 Graph String String (Record Types.Edge) String
--foreign import _updateEdge :: EffectFn4 Graph String String (Record Types.Edge) String
foreign import _mapNodes :: forall a. Fn2 Graph (Record Types.Node -> a) (Array a)
foreign import _forEachEdge :: EffectFn2 Graph (Record Types.Edge -> Effect Unit) Unit
foreign import _mapEdges :: forall a. Fn2 Graph (Record Types.Edge -> a) (Array a)

newGraph :: Unit -> Effect Graph
newGraph = runEffectFn1 _newGraph

graphFromSigmaxGraph :: Types.Graph Types.Node Types.Edge -> Effect Graph
graphFromSigmaxGraph (Types.Graph g) = do
  graph <- newGraph unit
  _ <- traverse (addNode graph) nodes
  _ <- traverse (addEdge graph) edges
  pure graph
  where
    nodes = A.fromFoldable g.nodes
    edges = A.fromFoldable g.edges

addNode :: Graph -> Record Types.Node -> Effect String
addNode g node@{ id } = runEffectFn3 _addNode g id node
removeNode :: Graph -> String -> Effect Unit
removeNode g nId = pure $ g ... "dropNode" $ [nId]
updateNode :: Graph -> Record Types.Node -> Effect Unit
-- | See Types.compareNodes
updateNode g node@{ id, hidden, highlighted } =
  runEffectFn3 _updateNode g id (\n -> n { hidden = hidden
                                         , highlighted = highlighted })
forEachNode :: Graph -> (Record Types.Node -> Effect Unit) -> Effect Unit
-- TODO Check this: how does FFI translate function of two arguments
-- into PS \x y ?
forEachNode g fn = pure $ g ... "forEachNode" $ [\_ n -> fn n]
mapNodes :: forall a. Graph -> (Record Types.Node -> a) -> Array a
mapNodes = runFn2 _mapNodes

addEdge :: Graph -> Record Types.Edge -> Effect String
addEdge g edge@{ source, target } = runEffectFn4 _addEdge g source target edge
removeEdge :: Graph -> String -> Effect Unit
removeEdge g eId = pure $ g ... "dropEdge" $ [eId]
updateEdge :: Graph -> Record Types.Edge -> Effect Unit
updateEdge _ _ = pure unit  -- TODO
--updateEdge g edge@{ source, target } = runEffectFn4 _updateEdge g source target edge
forEachEdge :: Graph -> (Record Types.Edge -> Effect Unit) -> Effect Unit
forEachEdge = runEffectFn2 _forEachEdge
--forEachEdge g fn = pure $ g ... "forEachEdge" $ [\_ e -> fn e]
mapEdges :: forall a. Graph -> (Record Types.Edge -> a) -> Array a
mapEdges = runFn2 _mapEdges

-- TODO Maybe our use of this function (`updateWithGraph`) in code is
-- too much. We convert Types.Graph into Graphology.Graph and then
-- update Sigma.graph with this.

-- NOTE: See `sigmax.performDiff`

-- | Since we don't want to replace directly the sigma.graph, we call
-- update.  This "intelligently" scans the `target` graph and updates
-- so that in the end it is the same as `source`.
updateWithGraph :: Graph -> Graph -> Effect Graph
-- TODO Add updateEdgesFromGraph
updateWithGraph target source = updateNodesFromGraph target source

-- | Update `target` graph so that it's `.nodes` is the same as that
-- | of `source` (with attributes as well)
updateNodesFromGraph :: Graph -> Graph -> Effect Graph
-- TODO Fixme
updateNodesFromGraph source _target = pure source
-- updateNodesFromGraph target source =
--   forEachNode target $ node@{ id } -> do
--     if Set.member id sourceNodeIds then
--         -- update node
--   updateNodes (removeNodes (addNodes target missingNodes) newNodes) nodesToUpdate
--   where
--     sourceNodeIds = nodeIds source
--     sourceEdgeIds = edgeIds source
--     targetNodeIds = nodeIds target
--     targetEdgeIds = edgeIds target
--     newNodeIds = Set.difference sourceNodeIds targetNodeIds
--     missingNodeIds = Set.difference targetNodeIds sourceNodeIds

-- | Clear a graphology graph.
clear :: Graph -> Effect Unit
clear g = pure $ g ... "clear" $ []

edges_ :: Graph -> Array Types.EdgeId
edges_ g = g ... "edges" $ [] :: Array Types.EdgeId
nodes_ :: Graph -> Array Types.NodeId
nodes_ g = g ... "nodes" $ [] :: Array Types.NodeId

-- | `sigma.edges()` returns only edge keys, we need to map to get the full edge
edges :: Graph -> Seq.Seq (Record Types.Edge)
edges g = Seq.fromFoldable $ mapEdges g identity
-- | `sigma.nodes()` returns only node keys, we need to map to get the full node
nodes :: Graph -> Seq.Seq (Record Types.Node)
nodes g = Seq.fromFoldable $ mapNodes g identity

-- | Fetch ids of graph edges in a sigmajs instance.
edgeIds :: Graph -> Types.EdgeIds
-- auto-assigned edge ids are different from our edge ids
edgeIds g = Set.fromFoldable $ mapEdges g (\{ id } -> id) -- -<<< edges_

-- | Fetch ids of graph nodes in a sigmajs instance.
nodeIds :: Graph -> Types.NodeIds
nodeIds = Set.fromFoldable <<< nodes_


-- | Read graph into a graphology instance.
-- graphRead :: forall nodeExtra node edgeExtra edge.
--              NodeProps nodeExtra node => EdgeProps edgeExtra edge =>
--              SigmaGraph -> Graph node edge -> Effect (Either EEx.Error Unit)
-- graphRead sg g = EEx.try $ pure $ sg ... "read" $ [ g ]


--type Graph n e = { nodes :: Array {|n}, edges :: Array {|e} }
