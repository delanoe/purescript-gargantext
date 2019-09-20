{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage (depending on your Config):
toUrl Back  Corpus 1 == "http://localhost:8008/api/v1.0/corpus/1"
toUrl Front Corpus 1 == "http://localhost:2015/#/corpus/1"
-}
module Gargantext.Config where

import Prelude
import Control.Plus (empty)
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject)
import Data.Array (filter, head)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Partial.Unsafe (unsafePartial)

import Gargantext.Router as R
import Gargantext.Types (TermList, TermSize(..))

data PathType = BackendPath | FrontendPath | StaticPath

class Path t where
  pathType :: t -> PathType
  path :: t -> String

url :: forall t. Path t => Ends -> t -> String
url e p = h (pathType p)
  where
    h BackendPath = back e.backend (path p)
    h FrontendPath = front e.frontend (path p)
    h StaticPath = front e.static (path p)
    back e path = e.baseUrl <> e.prePath <> show e.version <> "/" <> path
    front e path = e.baseUrl <> e.prePath <> path

type Backend =
  { name :: String,    version :: ApiVersion
  , prePath :: String, baseUrl :: String
  }

backendKey :: Backend -> String
backendKey {prePath, baseUrl} = prePath <> baseUrl

type Frontend = { name :: String, baseUrl :: String, prePath :: String }

backend :: ApiVersion -> String -> String -> String -> Backend
backend version baseUrl prePath name = { name, version, prePath, baseUrl }

frontend :: String -> String -> String -> Frontend
frontend baseUrl prePath name = { name, baseUrl, prePath }

defaultBackends :: NonEmpty Array Backend
defaultBackends = prod :| [dev, demo, local]
  where
    prod = backend V10 "http://gargantext.org" "/api/" "gargantext.org"
    dev = backend V10 "http://dev.gargantext.org" "/api/" "gargantext.org (dev)"
    demo = backend V10 "http://demo.gargantext.org" "/api/" "gargantext.org (demo)"
    local = backend V10 "http://localhost:8008" "/api/" "local"

defaultFrontends :: NonEmpty Array Frontend
defaultFrontends = relative :| [prod, dev, demo, haskell, caddy]
  where
    relative = frontend "" "/" "Relative"
    prod = frontend "https://gargantext.org" "/#/" "gargantext.org"
    dev = frontend "https://dev.gargantext.org" "/#/" "gargantext.org (dev)"
    demo = frontend "https://demo.gargantext.org" "/#/" "gargantext.org (demo)"
    haskell = frontend "http://localhost:8008" "/#/" "local (gargantext)"
    python = frontend "http://localhost:8000" "/#/" "local (python)"
    caddy = frontend "http://localhost:2015" "/#/" "local (caddy)"

defaultStatics :: NonEmpty Array Frontend
defaultStatics = relative :| []
  where
    relative = frontend "" "/" "relative"

type Ends =
  { backend  :: Backend
  , frontend :: Frontend
  , static   :: Frontend }

type Ends' =
  { backend  :: NonEmpty Array Backend
  , frontend :: NonEmpty Array Frontend
  , static   :: NonEmpty Array Frontend }

defaultEnds :: Ends
defaultEnds =
  { backend:  NonEmpty.head defaultBackends
  , frontend: NonEmpty.head defaultFrontends
  , static:   NonEmpty.head defaultStatics }

defaultEnds' :: Ends'
defaultEnds' =
  { backend:  defaultBackends
  , frontend: defaultFrontends
  , static:   defaultStatics }

limitUrl :: Limit -> String
limitUrl l = "&limit=" <> show l

offsetUrl :: Offset -> String
offsetUrl o = "&offset=" <> show o

orderUrl :: forall a. Show a => Maybe a -> String
orderUrl = maybe "" (\x -> "&order=" <> show x)

orderByUrl :: forall a. Show a => Maybe a -> String
orderByUrl = maybe "" (\x -> "&orderBy=" <> show x)

showTabType' :: TabType -> String
showTabType' (TabCorpus   t) = show t
showTabType' (TabDocument t) = show t
showTabType' (TabPairing t) = show t

data TabPostQuery = TabPostQuery {
    offset :: Int
  , limit :: Int
  , orderBy :: OrderBy
  , tabType :: TabType
  , query :: String
  }

instance encodeJsonTabPostQuery :: EncodeJson TabPostQuery where
  encodeJson (TabPostQuery post) =
        "view"       := showTabType' post.tabType
     ~> "offset"     := post.offset
     ~> "limit"      := post.limit
     ~> "orderBy"    := show post.orderBy
     ~> "query"      := post.query
     ~> jsonEmptyObject


------------------------------------------------------------

instance pathRoutes :: Path R.Routes where
  pathType _ = FrontendPath
  path = routesPath

routesPath :: R.Routes -> String
routesPath R.Home = ""
routesPath R.Login = "login"
routesPath (R.Folder i) = "folder/" <> show i
routesPath (R.Corpus i) = "corpus/" <> show i
routesPath (R.CorpusDocument c l i) = "corpus/" <> show c <> "/list/" <> show l <> "/document/" <> show i
routesPath (R.Document l i) = "list/" <> show l <> "/document/" <> show i
routesPath (R.PGraphExplorer i) = "#/"
routesPath (R.Texts i) = "texts/" <> show i
routesPath (R.Lists i) = "lists/" <> show i
routesPath R.Dashboard = "dashboard"
routesPath (R.Annuaire i) = "annuaire/" <> show i
routesPath (R.UserPage i) = "user/" <> show i
routesPath (R.ContactPage i) = "contact/" <> show i

-- nodeTypePath :: NodeType -> Path
-- nodeTypePath = NodeAPI

-- instance toUrlNodeType :: ToUrl NodeType where
--   toUrl ec e nt i = toUrl ec e (NodeAPI nt) i

-- instance toUrlPath :: ToUrl Path where
--   toUrl ec e p i = doUrl base path params
--     where
--       base   = endBaseUrl e ec
--       path   = endPathUrl e ec p i
--       params = ""
------------------------------------------------------------

data NodeType = NodeUser
              | Annuaire
              | NodeContact
              | Corpus
              | Url_Document
              | CorpusV3
              | Dashboard
              | Error
              | Folder
              | Graph
              | Phylo
              | Individu
              | Node
              | Nodes
              | Tree
              | NodeList
              | Texts

derive instance eqNodeType :: Eq NodeType

instance showNodeType :: Show NodeType where
  show NodeUser      = "NodeUser"
  show Annuaire      = "Annuaire"
  show NodeContact   = "NodeContact"
  show Corpus        = "NodeCorpus"
  show CorpusV3      = "NodeCorpusV3"
  show Dashboard     = "NodeDashboard"
  show Url_Document  = "NodeDocument"
  show Error         = "NodeError"
  show Folder        = "NodeFolder"
  show Graph         = "NodeGraph"
  show Phylo         = "NodePhylo"
  show Individu      = "NodeIndividu"
  show Node          = "Node"
  show Nodes         = "Nodes"
  show Tree          = "NodeTree"
  show NodeList      = "NodeList"
  show Texts         = "NodeTexts"

readNodeType :: String -> NodeType
readNodeType "NodeAnnuaire"  = Annuaire
readNodeType "NodeDashboard" = Dashboard
readNodeType "Document"      = Url_Document
readNodeType "NodeFolder"    = Folder
readNodeType "NodeGraph"     = Graph
readNodeType "NodePhylo"     = Phylo
readNodeType "Individu"      = Individu
readNodeType "Node"          = Node
readNodeType "Nodes"         = Nodes
readNodeType "NodeCorpus"    = Corpus
readNodeType "NodeCorpusV3"  = CorpusV3
readNodeType "NodeUser"      = NodeUser
readNodeType "NodeContact"   = NodeContact
readNodeType "Tree"          = Tree
readNodeType "NodeList"      = NodeList
readNodeType "NodeTexts"     = Texts
readNodeType _               = Error
{-
------------------------------------------------------------
instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
-}
------------------------------------------------------------
instance decodeJsonNodeType :: DecodeJson NodeType where
  decodeJson json = do
    obj <- decodeJson json
    pure $ readNodeType obj

instance encodeJsonNodeType :: EncodeJson NodeType where
  encodeJson nodeType = encodeJson $ show nodeType

nodeTypePath :: NodeType -> String
nodeTypePath Annuaire  = "annuaire"
nodeTypePath Corpus    = "corpus"
nodeTypePath CorpusV3  = "corpus"
nodeTypePath Dashboard = "dashboard"
nodeTypePath Url_Document  = "document"
nodeTypePath Error     = "ErrorNodeType"
nodeTypePath Folder    = "folder"
nodeTypePath Graph     = "graph"
nodeTypePath Phylo     = "phylo"
nodeTypePath Individu  = "individu"
nodeTypePath Node      = "node"
nodeTypePath Nodes     = "nodes"
nodeTypePath NodeUser  = "user"
nodeTypePath NodeContact = "contact"
nodeTypePath Tree      = "tree"
nodeTypePath NodeList  = "lists"
nodeTypePath Texts     = "texts"
------------------------------------------------------------

type ListId = Int

type NgramsGetOpts =
  { tabType        :: TabType
  , offset         :: Offset
  , limit          :: Limit
  , orderBy        :: Maybe OrderBy
  , listIds        :: Array ListId
  , termListFilter :: Maybe TermList
  , termSizeFilter :: Maybe TermSize
  , searchQuery    :: String
  }

type SearchOpts =
  { {-id :: Int
    , query    :: Array String
    ,-}
    listId   :: Int
  , limit    :: Limit
  , offset   :: Offset
  , orderBy  :: Maybe OrderBy
  }

type CorpusMetricOpts =
  { tabType :: TabType
  , listId  :: ListId
  , limit   :: Maybe Limit
  }

type ChartOpts =
  { chartType :: ChartType
  , tabType   :: TabType
  -- , listId  :: ListId
  -- , limit   :: Maybe Limit
  }

data BackendRoute
  = Auth
  | Tab TabType (Maybe Id)
  | Children NodeType Offset Limit (Maybe OrderBy) (Maybe Id)
  | GetNgrams NgramsGetOpts (Maybe Id)
  | PutNgrams TabType (Maybe ListId) (Maybe TermList) (Maybe Id)
  -- ^ The name is not good. In particular this URL is used both in PUT and POST.
  | NodeAPI NodeType (Maybe Id)
  | ListDocument (Maybe ListId) (Maybe Id)
  | Search SearchOpts (Maybe Id)
  | CorpusMetrics CorpusMetricOpts  (Maybe Id)
  | Chart ChartOpts (Maybe Id)

instance pathBackendRoute :: Path BackendRoute where
  pathType _ = BackendPath
  path = backendPath

backendPath :: BackendRoute -> String
backendPath (Tab t i) = backendPath (NodeAPI Node i) <> "/" <> showTabType' t
backendPath (Children n o l s i) = root <> "children?type=" <> show n <> offsetUrl o <> limitUrl l <> orderUrl s
  where root = backendPath (NodeAPI Node i) <> "/"
backendPath (NodeAPI Phylo pId) = "phyloscape?nodeId=" <> (show $ maybe 0 identity pId)
backendPath (GetNgrams opts i) =
  base opts.tabType
  <> "/ngrams?ngramsType="
  <> showTabType' opts.tabType
  <> offsetUrl opts.offset
  <> limitUrl opts.limit
  <> orderByUrl opts.orderBy
  <> foldMap (\x -> "&list=" <> show x) opts.listIds
  <> foldMap (\x -> "&listType=" <> show x) opts.termListFilter
  <> foldMap termSizeFilter opts.termSizeFilter
  <> search opts.searchQuery
  where
    base (TabCorpus _) = backendPath (NodeAPI Node i)
    base _ = backendPath (NodeAPI Url_Document i)
    termSizeFilter MonoTerm = "&minTermSize=0&maxTermSize=1"
    termSizeFilter MultiTerm = "&minTermSize=2"
    search "" = ""
    search s = "&search=" <> s
backendPath (ListDocument lId dId) =
  backendPath (NodeAPI NodeList lId) <> "/document/" <> (show $ maybe 0 identity dId)
backendPath (PutNgrams t listId termList i) =
    backendPath (NodeAPI Node i)
    <> "/ngrams?ngramsType="
    <> showTabType' t
    <> maybe "" (\x -> "&list=" <> show x) listId
    <> foldMap (\x -> "&listType=" <> show x) termList
backendPath Auth = "auth"
backendPath (NodeAPI nt i) = nodeTypePath nt <> (maybe "" (\i' -> "/" <> show i') i)
backendPath (Search {listId,limit,offset,orderBy} i) =
    backendPath (NodeAPI Corpus i)
    <> "/search?list_id=" <> show listId
    <> offsetUrl offset
    <> limitUrl limit
    <> orderUrl orderBy
backendPath (CorpusMetrics {tabType, listId, limit} i) =
    backendPath (NodeAPI Corpus i) <> "/metrics"
      <> "?ngrams=" <> show listId
      <> "&ngramsType=" <> showTabType' tabType
      <> maybe "" (\x -> "&limit=" <> show x) limit
-- TODO fix this url path
backendPath (Chart {chartType, tabType} i) =
    backendPath (NodeAPI Corpus i) <> "/" <> show chartType
      <> "?ngramsType=" <> showTabType' tabType
      <> "&listType=GraphTerm" -- <> show listId
      -- <> maybe "" (\x -> "&limit=" <> show x) limit

data NodePath = NodePath NodeType (Maybe Id)

instance pathNodePath :: Path NodePath where
  pathType _ = FrontendPath
  path (NodePath nt i) = nodeTypePath nt <> id
    where id = maybe "" (\i' -> "/" <> show i') i

data ChartType = Histo | Scatter | ChartPie | ChartTree

instance showChartType :: Show ChartType
  where
    show Histo    = "chart"
    show Scatter  = "scatter"
    show ChartPie = "pie"
    show ChartTree = "tree"

type Id  = Int
type Limit  = Int
type Offset = Int
data OrderBy = DateAsc  | DateDesc
             | TitleAsc | TitleDesc
             | ScoreAsc | ScoreDesc
             | TermAsc  | TermDesc
             | SourceAsc | SourceDesc

derive instance genericOrderBy :: Generic OrderBy _

instance showOrderBy :: Show OrderBy where
  show = genericShow

------------------------------------------------------------
data ApiVersion = V10 | V11
instance showApiVersion :: Show ApiVersion where
  show V10 = "v1.0"
  show V11 = "v1.1"
------------------------------------------------------------

instance eqApiVersion :: Eq ApiVersion where
  eq V10 V10 = true
  eq V11 V11 = true
  eq _ _ = false

data CTabNgramType = CTabTerms | CTabSources | CTabAuthors | CTabInstitutes

derive instance eqCTabNgramType :: Eq CTabNgramType

instance showCTabNgramType :: Show CTabNgramType where
  show CTabTerms      = "Terms"
  show CTabSources    = "Sources"
  show CTabAuthors    = "Authors"
  show CTabInstitutes = "Institutes"

data PTabNgramType = PTabPatents | PTabBooks | PTabCommunication

derive instance eqPTabNgramType :: Eq PTabNgramType

instance showPTabNgramType :: Show PTabNgramType where
  show PTabPatents       = "Patents"
  show PTabBooks         = "Books"
  show PTabCommunication = "Communication"

data TabSubType a = TabDocs | TabNgramType a | TabTrash | TabMoreLikeFav | TabMoreLikeTrash

derive instance eqTabSubType :: Eq a => Eq (TabSubType a)

instance showTabSubType :: Show a => Show (TabSubType a) where
  show TabDocs          = "Docs"
  show (TabNgramType a) = show a
  show TabTrash         = "Trash"
  show TabMoreLikeFav   = "MoreFav"
  show TabMoreLikeTrash = "MoreTrash"

data TabType
  = TabCorpus   (TabSubType CTabNgramType)
  | TabPairing  (TabSubType PTabNgramType)
  | TabDocument (TabSubType CTabNgramType)

derive instance eqTabType :: Eq TabType

derive instance genericTabType :: Generic TabType _

instance showTabType :: Show TabType where
  show = genericShow

