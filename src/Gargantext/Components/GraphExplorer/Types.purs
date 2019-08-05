module Gargantext.Components.GraphExplorer.Types where

import Prelude
import Partial.Unsafe (unsafePartial)

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (concat, fromFoldable, group, sort, take, (!!), length)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Newtype (class Newtype)

newtype Node = Node
  { id_ :: String
  , size :: Int
  , type_ :: String
  , label :: String
  , x :: Number
  , y :: Number
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

type ListId      = Int
type CorpusId    = Int
type CorpusLabel = String

newtype GraphSideCorpus = GraphSideCorpus
  { corpusId    :: CorpusId
  , corpusLabel :: CorpusLabel
  , listId      :: ListId
  }

newtype GraphData = GraphData
  { nodes :: Array Node
  , edges :: Array Edge
  , sides :: Array GraphSideCorpus
  , metaData :: Maybe MetaData
  }

derive instance newtypeGraphData :: Newtype GraphData _


newtype MetaData = MetaData
  {
    title :: String
  , legend :: Array Legend
  , corpusId :: Array Int
  , listId   :: ListId
  }


instance decodeJsonGraphData :: DecodeJson GraphData where
  decodeJson json = do
    obj <- decodeJson json
    nodes <- obj .? "nodes"
    edges <- obj .? "edges"
    -- TODO: sides
    metadata <- obj .? "metadata"
    corpusIds <- metadata .? "corpusId"
    listId'   <- metadata .? "listId"
    metaData <- obj .? "metadata"
    let side x = GraphSideCorpus { corpusId: x, corpusLabel: "Publications", listId : listId'}
    let sides = side <$> corpusIds
    pure $ GraphData { nodes, edges, sides, metaData }

instance decodeJsonNode :: DecodeJson Node where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    type_ <- obj .? "type"
    label <- obj .? "label"
    size <- obj .? "size"
    attributes <- obj .? "attributes"
    x <- obj .? "x_coord"
    y <- obj .? "y_coord"
    pure $ Node { id_, type_, size, label, attributes, x, y }


instance decodeJsonMetaData :: DecodeJson MetaData where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    legend <- obj .? "legend"
    corpusId <- obj .? "corpusId"
    listId <- obj .? "listId"
    pure $ MetaData { title, legend, corpusId, listId}


instance decodeJsonLegend :: DecodeJson Legend where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    color <- obj .? "color"
    label <- obj .? "label"
    pure $ Legend { id_, color, label }


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

newtype Legend = Legend  {id_ ::Int , color :: String, label :: String}

instance eqLegend :: Eq Legend where
  eq (Legend l1) (Legend l2) = eq l1.id_ l2.id_

instance ordLegend :: Ord Legend where
  compare (Legend l1) (Legend l2) = compare l1.id_ l2.id_

getLegendData :: GraphData -> Array Legend
getLegendData (GraphData {nodes, edges, metaData}) = getLegend metaData
  where
    getLegend (Just (MetaData {legend})) = legend
    getLegend Nothing  = []


defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)


intColor :: Int -> String
intColor i = unsafePartial $ fromJust $ defaultPalette !! (i `mod` length defaultPalette)
