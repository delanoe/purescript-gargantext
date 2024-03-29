module Gargantext.Components.GraphExplorer.Types where

import Gargantext.Prelude

import Data.Array ((!!), length)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Simple.JSON as JSON
import Gargantext.Components.GraphExplorer.GraphTypes
import Gargantext.Hooks.Sigmax.Camera (Camera(..))
import Type.Proxy (Proxy(..))

type GraphId = Int



-- | A 'fully closed interval' in CS parlance
type InclusiveRange t = { min :: t, max :: t }

type ListId      = Int
type Version     = Int
type CorpusId    = Int
type CorpusLabel = String
type DocId       = Int

newtype GraphSideCorpus = GraphSideCorpus
  { corpusId    :: CorpusId
  , corpusLabel :: CorpusLabel
  , listId      :: ListId
  }
derive instance Generic GraphSideCorpus _
instance Eq GraphSideCorpus where eq = genericEq

newtype GraphSideDoc = GraphSideDoc
  { docId     :: DocId
  , corpusId  :: CorpusId
  , listId    :: ListId
  }
derive instance Newtype GraphSideDoc _
derive instance Generic GraphSideDoc _
instance Eq GraphSideDoc where eq = genericEq

newtype GraphData = GraphData
  { nodes :: Array Node
  , edges :: Array Edge
  , sides :: Array GraphSideCorpus
  , metaData :: Maybe MetaData
  }
derive instance Newtype GraphData _
derive instance Generic GraphData _
instance Eq GraphData where eq = genericEq
instance JSON.ReadForeign GraphData where
  readImpl f = do
    inst :: { nodes :: Array Node
            , edges :: Array Edge
            , metadata :: MetaData } <- JSON.readImpl f
    let (MetaData metadata) = inst.metadata
    let side x = GraphSideCorpus { corpusId: x
                                 , corpusLabel: "Publications"
                                 , listId : metadata.list.listId }
    let sides = side <$> metadata.corpusId
    pure $ GraphData { nodes: inst.nodes
                     , edges: inst.edges
                     , sides
                     , metaData: Just inst.metadata }
instance JSON.WriteForeign GraphData where
  writeImpl (GraphData gd) = JSON.writeImpl { nodes: gd.nodes
                                            , edges: gd.edges
                                            , metadata: gd.metaData }

newtype MetaData = MetaData
  { corpusId :: Array Int
  , legend   :: Array Legend
  , list :: { listId  :: ListId
            , version :: Version
            }
  , metric :: String  -- dummy value
  , startForceAtlas :: Boolean
  , title    :: String
  }
derive instance Generic MetaData _
derive instance Newtype MetaData _
instance Eq MetaData where eq = genericEq
derive newtype instance JSON.ReadForeign MetaData
derive newtype instance JSON.WriteForeign MetaData

getLegend :: GraphData -> Maybe (Array Legend)
getLegend (GraphData {metaData}) = (\(MetaData m) -> m.legend) <$> metaData

newtype SelectedNode = SelectedNode {id :: String, label :: String}

derive instance Generic SelectedNode _
derive instance Newtype SelectedNode _
instance Eq SelectedNode where eq = genericEq
instance Ord SelectedNode where compare = genericCompare

instance Show SelectedNode where show (SelectedNode node) = node.label

-- type State = (
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
  -- )

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

newtype Legend = Legend  {id_ ::Int , color :: String, label :: String}

derive instance Generic Legend _
derive instance Newtype Legend _
instance Eq Legend where eq (Legend l1) (Legend l2) = eq l1.id_ l2.id_
instance Ord Legend where compare (Legend l1) (Legend l2) = compare l1.id_ l2.id_
instance JSON.ReadForeign Legend where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ Legend $ Record.rename idP id_P inst
instance JSON.WriteForeign Legend where
  writeImpl (Legend l) = JSON.writeImpl $ Record.rename id_P idP l


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

newtype HyperdataGraph = HyperdataGraph {
    graph   :: GraphData
  , mCamera :: Maybe Camera
  }
derive instance Generic HyperdataGraph _
derive instance Newtype HyperdataGraph _
instance Eq HyperdataGraph where eq = genericEq
instance JSON.ReadForeign HyperdataGraph where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ HyperdataGraph $ Record.rename cameraP mCameraP inst
instance JSON.WriteForeign HyperdataGraph where
  writeImpl (HyperdataGraph c) = JSON.writeImpl $ Record.rename mCameraP cameraP c

data Stage = Init | Ready | Cleanup
derive instance Generic Stage _
derive instance Eq Stage

-----------------------------------------------------------------------

newtype CacheParams = CacheParams
  { expandSelection     :: Boolean
  , expandNeighborhood  :: Boolean
  }

derive instance Newtype CacheParams _
derive instance Generic CacheParams _
derive instance Eq CacheParams
instance Show CacheParams where show = genericShow
derive newtype instance JSON.ReadForeign CacheParams
derive newtype instance JSON.WriteForeign CacheParams

-- (!) in case cache storage (ie. JavaScript Local Storage) returns an invalid
--     objects (eg. possible data migration), this will safely set new default
--     values
defaultCacheParams :: CacheParams
defaultCacheParams = CacheParams
  { expandSelection   : true
  , expandNeighborhood: true
  }
