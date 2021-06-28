module Gargantext.Components.GraphExplorer.Types where

import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson, (.:), (.:?), jsonEmptyObject, (~>), (:=))
import Data.Array ((!!), length)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Ord
import Partial.Unsafe (unsafePartial)

import Gargantext.Prelude

type GraphId = Int

newtype Node = Node {
    attributes :: Cluster
  , id_ :: String
  , label :: String
  , size :: Int
  , type_ :: String
  , x :: Number
  , y :: Number
  }

derive instance Generic Node _
derive instance Newtype Node _
instance Eq Node where
  eq = genericEq
instance Ord Node where
  compare (Node n1) (Node n2) = compare n1.id_ n2.id_

newtype Cluster = Cluster { clustDefault :: Int }

derive instance Generic Cluster _
derive instance Newtype Cluster _
instance Eq Cluster where
  eq = genericEq

newtype Edge = Edge {
    confluence :: Number
  , id_ :: String
  , source :: String
  , target :: String
  , weight :: Number
  }

derive instance Generic Edge _
derive instance Newtype Edge _
instance Eq Edge where
  eq = genericEq
instance Ord Edge where
  compare (Edge e1) (Edge e2) = compare e1.id_ e2.id_

-- | A 'fully closed interval' in CS parlance
type InclusiveRange t = { min :: t, max :: t }

type ListId      = Int
type Version     = Int
type CorpusId    = Int
type CorpusLabel = String

newtype GraphSideCorpus = GraphSideCorpus
  { corpusId    :: CorpusId
  , corpusLabel :: CorpusLabel
  , listId      :: ListId
  }
derive instance Generic GraphSideCorpus _
instance Eq GraphSideCorpus where
  eq = genericEq

newtype GraphData = GraphData
  { nodes :: Array Node
  , edges :: Array Edge
  , sides :: Array GraphSideCorpus
  , metaData :: Maybe MetaData
  }
derive instance Newtype GraphData _
derive instance Generic GraphData _
instance Eq GraphData where
  eq = genericEq


newtype MetaData = MetaData
  { corpusId :: Array Int
  , legend   :: Array Legend
  , list :: { listId   :: ListId
           , version  :: Version
           }
  , metric :: String  -- dummy value
  , startForceAtlas :: Boolean
  , title    :: String
  }
derive instance Generic MetaData _
instance Eq MetaData where
  eq = genericEq

getLegend :: GraphData -> Maybe (Array Legend)
getLegend (GraphData {metaData}) = (\(MetaData m) -> m.legend) <$> metaData

newtype SelectedNode = SelectedNode {id :: String, label :: String}

derive instance Eq SelectedNode
derive instance Newtype SelectedNode _
derive instance Ord SelectedNode

instance Show SelectedNode where
  show (SelectedNode node) = node.label

type State = (
  --  corpusId :: R.State Int
  --, filePath :: R.State String
  --, graphData :: R.State GraphData
  --, legendData :: R.State (Array Legend)
  --, multiNodeSelection :: R.State Boolean
  --, selectedNodes :: R.State (Set SelectedNode)
  --, showControls :: T.Box Boolean
  --, showTree :: R.State Boolean
  --, sidePanelState :: R.State Boolean
  --, sigmaGraphData :: R.State (Maybe SigmaxTypes.SGraph)
  --, sigmaSettings :: R.State ({|Graph.SigmaSettings})
    --treeId :: R.State (Maybe TreeId)
  )

initialGraphData :: GraphData
initialGraphData = GraphData {
    nodes: []
  , edges: []
  , sides: []
  , metaData : Just $ MetaData {
       corpusId : []
     , legend : []
     , list: { listId : 0, version : 0 }
     , metric: "Order1"
     , startForceAtlas: true
     , title : ""
     }
  }

instance DecodeJson GraphData where
  decodeJson json = do
    obj <- decodeJson json
    nodes <- obj .: "nodes"
    edges <- obj .: "edges"
    -- TODO: sides
    metadata <- obj .: "metadata"
    corpusIds <- metadata .: "corpusId"
    list      <- metadata .: "list"
    listId'   <- list .: "listId"
    metaData <- obj .: "metadata"
    let side x = GraphSideCorpus { corpusId: x, corpusLabel: "Publications", listId : listId'}
    let sides = side <$> corpusIds
    pure $ GraphData { nodes, edges, sides, metaData }

instance EncodeJson GraphData where
  encodeJson (GraphData gd) =
       "nodes"    := gd.nodes
     ~> "edges"    := gd.edges
     ~> "metadata" := gd.metaData
     ~> jsonEmptyObject

instance DecodeJson Node where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .: "id"
    type_ <- obj .: "type"
    label <- obj .: "label"
    size  <- obj .: "size"
    attributes <- obj .: "attributes"
    x <- obj .: "x_coord"
    y <- obj .: "y_coord"
    pure $ Node { id_, type_, size, label, attributes, x, y }

instance EncodeJson Node where
  encodeJson (Node nd) =
       "id"         := nd.id_
     ~> "attributes" := nd.attributes
     ~> "label"      := nd.label
     ~> "size"       := nd.size
     ~> "type"       := nd.type_
     ~> "x_coord"    := nd.x
     ~> "y_coord"    := nd.y
     ~> jsonEmptyObject


instance DecodeJson MetaData where
  decodeJson json = do
    obj      <- decodeJson json
    legend   <- obj .: "legend"
    corpusId <- obj .: "corpusId"
    list     <- obj .: "list"
    listId   <- list .: "listId"
    metric   <- obj .: "metric"
    startForceAtlas <- obj .: "startForceAtlas"
    title   <- obj .: "title"
    version <- list .: "version"
    pure $ MetaData {
        corpusId
      , legend
      , list: {listId, version}
      , metric
      , startForceAtlas
      , title
    }

instance EncodeJson MetaData where
  encodeJson (MetaData md) =
       "corpusId"        := md.corpusId
     ~> "legend"          := md.legend
     ~> "list"            := md.list
     ~> "metric"          := md.metric
     ~> "startForceAtlas" := md.startForceAtlas
     ~> "title"           := md.title
     ~> jsonEmptyObject

instance DecodeJson Legend where
  decodeJson json = do
    obj <- decodeJson json
    id_   <- obj .: "id"
    color <- obj .: "color"
    label <- obj .: "label"
    pure $ Legend { id_, color, label }

instance EncodeJson Legend where
  encodeJson (Legend lg) =
       "id"    := lg.id_
     ~> "color" := lg.color
     ~> "label" := lg.label
     ~> jsonEmptyObject


instance DecodeJson Cluster where
  decodeJson json = do
    obj <- decodeJson json
    clustDefault <- obj .: "clust_default"
    pure $ Cluster { clustDefault }

instance EncodeJson Cluster where
  encodeJson (Cluster cl) =
       "clust_default" := cl.clustDefault
     ~> jsonEmptyObject

instance DecodeJson Edge where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .: "id"
    source <- obj .: "source"
    target <- obj .: "target"
    weight <- obj .: "weight"
    confluence <- obj .: "confluence"
    pure $ Edge { id_, source, target, weight, confluence }

instance EncodeJson Edge where
  encodeJson (Edge ed) =
       "id"         := ed.id_
     ~> "confluence" := ed.confluence
     ~> "source"     := ed.source
     ~> "target"     := ed.target
     ~> "weight"     := ed.weight
     ~> jsonEmptyObject

newtype Legend = Legend  {id_ ::Int , color :: String, label :: String}

instance Eq Legend where
  eq (Legend l1) (Legend l2) = eq l1.id_ l2.id_

instance Ord Legend where
  compare (Legend l1) (Legend l2) = compare l1.id_ l2.id_

getLegendData :: GraphData -> Array Legend
getLegendData (GraphData {metaData: Just (MetaData {legend})}) = legend
getLegendData _ = []

defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)


intColor :: Int -> String
intColor i = unsafePartial $ fromJust $ defaultPalette !! (i `mod` length defaultPalette)

data SideTab = SideTabLegend | SideTabData | SideTabCommunity

derive instance Eq SideTab
instance Show SideTab where
  show SideTabLegend    = "Legend"
  show SideTabData      = "Data"
  show SideTabCommunity = "Community"


newtype Camera =
  Camera { ratio :: Number
         , x     :: Number
         , y     :: Number
         }
derive instance Generic Camera _
instance Eq Camera where
  eq = genericEq
instance DecodeJson Camera where
  decodeJson json = do
    obj   <- decodeJson json
    ratio <- obj .: "ratio"
    x     <- obj .: "x"
    y     <- obj .: "y"
    pure $ Camera { ratio, x, y }
instance EncodeJson Camera where
  encodeJson (Camera c) =
       "ratio" := c.ratio
     ~> "x"     := c.x
     ~> "y"     := c.y
     ~> jsonEmptyObject


newtype HyperdataGraph = HyperdataGraph {
    graph   :: GraphData
  , mCamera :: Maybe Camera
  }
derive instance Generic HyperdataGraph _
instance Eq HyperdataGraph where
  eq = genericEq
instance DecodeJson HyperdataGraph where
  decodeJson json = do
    obj <- decodeJson json
    graph   <- obj .: "graph"
    mCamera <- obj .:? "camera"
    pure $ HyperdataGraph { graph, mCamera }
instance EncodeJson HyperdataGraph where
  encodeJson (HyperdataGraph c) =
      "camera"  := c.mCamera
     ~> "graph"  := c.graph
     ~> jsonEmptyObject
