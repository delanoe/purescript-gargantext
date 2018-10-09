module Gargantext.Components.GraphExplorer.Types where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (concat, fromFoldable, group, sort, take)
import Data.Newtype (class Newtype)

newtype Node = Node
  { id_ :: String
  , size :: Int
  , type_ :: String
  , label :: String
  , attributes :: Cluster
  }

derive instance newtypeNode :: Newtype Node _

newtype Cluster = Cluster { clustDefault :: Int }

derive instance newtypeCluster :: Newtype Cluster _

newtype Edge = Edge
  { id_ :: String
  , source :: String
  , target :: String
  , weight :: Number
  }

derive instance newtypeEdge :: Newtype Edge _

newtype GraphData = GraphData
  { nodes :: Array Node
  , edges :: Array Edge
  }

derive instance newtypeGraphData :: Newtype GraphData _

instance decodeJsonGraphData :: DecodeJson GraphData where
  decodeJson json = do
    obj <- decodeJson json
    nodes <- obj .? "nodes"
    edges <- obj .? "edges"
    pure $ GraphData { nodes, edges }

instance decodeJsonNode :: DecodeJson Node where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    type_ <- obj .? "type"
    label <- obj .? "label"
    size <- obj .? "size"
    attributes <- obj .? "attributes"
    pure $ Node { id_, type_, size, label, attributes }

instance decodeJsonCluster :: DecodeJson Cluster where
  decodeJson json = do
    obj <- decodeJson json
    clustDefault <- obj .? "clust_default"
    pure $ Cluster { clustDefault }

instance decodeJsonEdge :: DecodeJson Edge where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    source <- obj .? "source"
    target <- obj .? "target"
    weight <- obj .? "weight"
    pure $ Edge { id_, source, target, weight }

newtype Legend = Legend  {id_ ::Int , label :: String}

instance eqLegend :: Eq Legend where
  eq (Legend l1) (Legend l2) = eq l1.id_ l2.id_

instance ordLegend :: Ord Legend where
  compare (Legend l1) (Legend l2) = compare l1.id_ l2.id_

getLegendData :: GraphData -> Array Legend
getLegendData (GraphData {nodes, edges}) = nn
  where
    --mp (NonEmptyArray a ary) = [a] <> (if length ary > 0 then [unsafePartial $ fromJust $ head ary] else [])
    n = sort $ map t' nodes
    g = group n
    nn = take 5 $ concat $ map fromFoldable g -- TODO: fix this after checking the output

t' :: Node -> Legend
t' (Node r) = Legend { id_ : clustDefault, label : r.label}
  where
    (Cluster {clustDefault}) = r.attributes
