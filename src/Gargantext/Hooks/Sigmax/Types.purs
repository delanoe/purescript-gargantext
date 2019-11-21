module Gargantext.Hooks.Sigmax.Types where

import Prelude (map, ($))
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
  , x     :: Number
  , y     :: Number
  , size  :: Number
  , color :: String )

type Edge = ( id :: String, source :: String, target :: String )

type SelectedNodeIds = Set.Set String
type NodesMap = Map.Map String (Record Node)

nodesMap :: Graph Node Edge -> NodesMap
nodesMap graph = do
  let (Graph {nodes}) = graph
  Map.fromFoldable $ map (\n -> Tuple n.id n) nodes
