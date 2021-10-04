module Gargantext.Data.Louvain.Algorithm where

import Data.Foldable (sum)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Prelude ((&&), (==), ($), (<$>), class Eq, class Ord)

newtype Cluster = Cluster String
newtype Node = Node String
derive instance Eq Node
derive instance Ord Node
newtype Edge = Edge {
    source :: Node
  , target :: Node
  , weight :: Number
  }
newtype Graph = Graph {
    edges :: Seq.Seq Edge
  , nodes :: Seq.Seq Node
  }

newtype Status = Status {
    --degrees
    --gdegrees
    --internals
    --loops
    --nodesToCom ::
    totalWeight :: Number
  }

newtype Dendrogram = Dendrogram (Map.Map Node Cluster)

-- edge helpers
eSource :: Edge -> Node
eSource (Edge {source}) = source

eTarget :: Edge -> Node
eTarget (Edge {target}) = target

eWeight :: Edge -> Number
eWeight (Edge {weight}) = weight

-- graph helpers
gEdges :: Graph -> Seq.Seq Edge
gEdges (Graph {edges}) = edges

gNodes :: Graph -> Seq.Seq Node
gNodes (Graph {nodes}) = nodes

getGraphSize :: Graph -> Number
getGraphSize g = sum $ Seq.map eWeight $ gEdges g

getEdgeWeight :: Graph -> Node -> Node -> Maybe Number
getEdgeWeight g n1 n2 = eWeight <$> Seq.head edges
  where
    edges = Seq.filter (\e -> eSource e == n1 && eTarget e == n2) (gEdges g)

getNeighboursOfNode :: Graph -> Node -> Seq.Seq Node
getNeighboursOfNode g n = Seq.fromFoldable $ Set.union sources targets
  where
    edges = gEdges g
    sourceEdges = Seq.filter (\e -> eSource e == n) edges
    targetEdges = Seq.filter (\e -> eTarget e == n) edges
    -- edge target, when edge source matches n
    sources = Set.fromFoldable $ Seq.map eTarget sourceEdges
    -- edge source, when edge target matches n
    targets = Set.fromFoldable $ Seq.map eSource targetEdges

-- TODO algorithm
initStatus :: Graph -> Status -> Maybe Dendrogram -> Status
initStatus g s Nothing = Status {totalWeight}
  where
    totalWeight = getGraphSize g
initStatus g s (Just d) = Status {totalWeight}
  where
    totalWeight = getGraphSize g

generateDendrogram :: Graph -> Dendrogram -> Dendrogram
generateDendrogram g partInit =
  if Seq.null (gEdges g) then
    Dendrogram $ Map.fromFoldable $ Seq.map (\n@(Node ns) -> Tuple n (Cluster ns)) (gNodes g)
  else
    Dendrogram $ Map.empty
