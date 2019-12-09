module Gargantext.Hooks.Sigmax.Types where

import Prelude (map, ($), (&&), (==))
import Data.Map as Map
import Data.Sequence (Seq)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import DOM.Simple.Types (Element)

newtype Graph n e = Graph { nodes :: Seq {|n}, edges :: Seq {|e} }

--derive instance eqGraph :: Eq Graph

--instance eqGraph :: Eq Graph where
--  eq (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = n1 == n2 && e1 == e2


type Renderer = { "type" :: String, container :: Element }


type Node =
  ( id    :: String
  , label :: String
  , hidden :: Boolean
  , x     :: Number
  , y     :: Number
  , size  :: Number
  , color :: String )

type Edge =
  ( id :: String
  , color :: String
  , size :: Number
  , source :: String
  , target :: String )

type SelectedNodeIds = Set.Set String
type SelectedEdgeIds = Set.Set String
type EdgesMap = Map.Map String (Record Edge)
type NodesMap = Map.Map String (Record Node)

edgesGraphMap :: Graph Node Edge -> EdgesMap
edgesGraphMap graph = do
  let (Graph {edges}) = graph
  Map.fromFoldable $ map (\e -> Tuple e.id e) edges

nodesMap :: Seq (Record Node) -> NodesMap
nodesMap nodes = Map.fromFoldable $ map (\n -> Tuple n.id n) nodes

nodesGraphMap :: Graph Node Edge -> NodesMap
nodesGraphMap graph = do
  let (Graph {nodes}) = graph
  nodesMap nodes

eqGraph :: (Graph Node Edge) -> (Graph Node Edge) -> Boolean
eqGraph (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = (n1 == n2) && (e1 == e2)
