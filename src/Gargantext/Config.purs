{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage (depending on your Config):
toUrl Back  Corpus 1 == "http://localhost:8008/api/v1.0/corpus/1"
toUrl Front Corpus 1 == "http://localhost:2015/#/corpus/1"
-}
module Gargantext.Config where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Gargantext.Router as R

import Gargantext.Types (TermList, TermSize(..))

urlPlease :: End -> String -> String
urlPlease end path = theEnd.baseUrl <> theEnd.prePath <> path
  where theEnd = endOf end endConfig

endConfig :: EndConfig
endConfig = endConfig' V10

endConfig' :: ApiVersion -> EndConfig
endConfig' v = { front : frontRelative
               , back  : backLocal v
               --, back: backDev v
               , static : staticRelative
             }
--               , back  : backDemo v  }

------------------------------------------------------------------------
frontRelative :: Config
frontRelative = { baseUrl: ""
                , prePath: "/#/"
                }

staticRelative :: Config
staticRelative = { baseUrl: ""
                 , prePath : "/"
               }

frontCaddy :: Config
frontCaddy = { baseUrl: "http://localhost:2015"
             , prePath: "/#/"
             }

frontHaskell :: Config
frontHaskell = { baseUrl: "http://localhost:8008"
               , prePath: "/#/"
               }

frontDev :: Config
frontDev = { baseUrl: "https://dev.gargantext.org"
           , prePath: "/#/"
           }

frontDemo :: Config
frontDemo = { baseUrl: "https://demo.gargantext.org"
           , prePath: "/#/"
           }

frontProd :: Config
frontProd = { baseUrl: "https://gargantext.org"
            , prePath: "/#/"
            }

------------------------------------------------------------------------

backLocal :: ApiVersion -> Config
backLocal v = { baseUrl: "http://localhost:8008"
              , prePath: "/api/" <> show v <> "/"
              }

backDev :: ApiVersion -> Config
backDev v = { baseUrl: "https://dev.gargantext.org"
            , prePath: "/api/" <> show v <> "/"
            }

backDemo :: ApiVersion -> Config
backDemo v = { baseUrl: "https://demo.gargantext.org"
             , prePath: "/api/" <> show v <> "/"
             }

backProd :: ApiVersion -> Config
backProd v = { baseUrl: "https://gargantext.org"
             , prePath: "/api/" <> show v <> "/"
             }
------------------------------------------------------------------------

type EndConfig = { front :: Config
                 , back  :: Config
                 , static :: Config
                 }

type Config = { baseUrl :: String
              , prePath :: String
              }

------------------------------------------------------------
type UrlBase  = String
type UrlPath  = String
type UrlParam = String
type Url      = String

doUrl :: UrlBase -> UrlPath -> UrlParam -> Url
doUrl b p ps = b <> p <> ps

endOf :: forall cfg. End -> { front :: cfg, back :: cfg, static :: cfg } -> cfg
endOf Back  = _.back
endOf Front = _.front
endOf Static = _.static

endBaseUrl :: End -> EndConfig -> UrlBase
endBaseUrl end c = (endOf end c).baseUrl

endPathUrl :: End -> EndConfig -> Path -> Maybe Id -> UrlPath
endPathUrl end = pathUrl <<< endOf end

limitUrl :: Limit -> UrlPath
limitUrl l = "&limit=" <> show l

offsetUrl :: Offset -> UrlPath
offsetUrl o = "&offset=" <> show o

orderUrl :: forall a. Show a => Maybe a -> UrlPath
orderUrl = maybe "" (\x -> "&order=" <> show x)

orderByUrl :: forall a. Show a => Maybe a -> UrlPath
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

pathUrl :: Config -> Path -> Maybe Id -> UrlPath
pathUrl c (Tab t) i =
    pathUrl c (NodeAPI Node) i <> "/" <> showTabType' t
pathUrl c (Children n o l s) i =
    pathUrl c (NodeAPI Node) i <>
      "/" <> "children?type=" <> show n <> offsetUrl o <> limitUrl l <> orderUrl s
pathUrl c (NodeAPI Phylo) pId = "phyloscape?nodeId=" <> (show $ maybe 0 identity pId)
pathUrl c (GetNgrams
            { tabType: t
            , offset: o
            , orderBy
            , limit: l
            , listIds
            , termListFilter: tlf
            , termSizeFilter: tsf
            , searchQuery: q
            }) i =
      base
      <> "/ngrams?ngramsType=" <> showTabType' t
      <> offsetUrl o <> limitUrl l
      <> orderByUrl orderBy
      <> foldMap (\x -> "&list=" <> show x) listIds
      <> foldMap (\x -> "&listType=" <> show x) tlf
      <> foldMap (\x -> case x of
                          MonoTerm  -> "&minTermSize=0&maxTermSize=1"
                          MultiTerm -> "&minTermSize=2"
                 ) tsf
      <> if q == "" then "" else ("&search=" <> q)
        where
          base =  case t of
                    TabCorpus _ -> pathUrl c (NodeAPI Node) i
                    _           -> pathUrl c (NodeAPI Url_Document) i
pathUrl c (ListDocument lId) dId =
  pathUrl c (NodeAPI NodeList) lId <> "/document/" <> (show $ maybe 0 identity dId)

pathUrl c (PutNgrams t listid termList) i =
    pathUrl c (NodeAPI Node) i <> "/ngrams?ngramsType="
      <> showTabType' t
      <> maybe "" (\x -> "&list=" <> show x) listid
      <> foldMap (\x -> "&listType=" <> show x) termList
pathUrl c Auth Nothing = c.prePath <> "auth"
pathUrl c Auth (Just _) = "impossible" -- TODO better types
pathUrl c (NodeAPI nt) i = c.prePath <> nodeTypeUrl nt <> (maybe "" (\i' -> "/" <> show i') i)
pathUrl c (Search {listId,limit,offset,orderBy}) i =
    pathUrl c (NodeAPI Corpus) i
      <> "/search?list_id=" <> show listId
      <> offsetUrl offset <> limitUrl limit <> orderUrl orderBy
pathUrl c (CorpusMetrics {tabType, listId, limit}) i =
    pathUrl c (NodeAPI Corpus) i <> "/metrics"
      <> "?ngrams=" <> show listId
      <> "&ngramsType=" <> showTabType' tabType
      <> maybe "" (\x -> "&limit=" <> show x) limit
-- TODO fix this url path
pathUrl c (Chart {chartType, tabType}) i =
    pathUrl c (NodeAPI Corpus) i <> "/" <> show chartType
      <> "?ngramsType=" <> showTabType' tabType
      <> "&listType=GraphTerm" -- <> show listId
      -- <> maybe "" (\x -> "&limit=" <> show x) limit


------------------------------------------------------------

routesPath :: R.Routes -> String
routesPath R.Home = ""
routesPath R.Login = "login"
routesPath R.SearchView = "search"
routesPath (R.Folder i) = "folder/" <> show i
routesPath (R.Corpus i) = "corpus/" <> show i
routesPath R.AddCorpus = "addCorpus"
routesPath (R.CorpusDocument c l i) = "corpus/" <> show c <> "/list/" <> show l <> "/document/" <> show i
routesPath (R.Document l i) = "list/" <> show l <> "/document/" <> show i
routesPath (R.PGraphExplorer i) = "#/"
routesPath (R.Texts i) = "texts/" <> show i
routesPath (R.Lists i) = "lists/" <> show i
routesPath R.Dashboard = "dashboard"
routesPath (R.Annuaire i) = "annuaire/" <> show i
routesPath (R.UserPage i) = "user/" <> show i
routesPath (R.ContactPage i) = "contact/" <> show i

class Linkable a where
  toLink :: a -> String

instance linkableRoutes :: Linkable R.Routes where
  toLink l = endConfig.front.baseUrl <> endConfig.front.prePath <> routesPath l

class ToUrl a where
  toUrl :: End -> a -> Maybe Id -> Url

instance toUrlNodeType :: ToUrl NodeType where
  toUrl e nt i = toUrl e (NodeAPI nt) i

instance toUrlPath :: ToUrl Path where
  toUrl e p i = doUrl base path params
    where
      base   = endBaseUrl e endConfig
      path   = endPathUrl e endConfig p i
      params = ""
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

nodeTypeUrl :: NodeType -> Url
nodeTypeUrl Annuaire  = "annuaire"
nodeTypeUrl Corpus    = "corpus"
nodeTypeUrl CorpusV3  = "corpus"
nodeTypeUrl Dashboard = "dashboard"
nodeTypeUrl Url_Document  = "document"
nodeTypeUrl Error     = "ErrorNodeType"
nodeTypeUrl Folder    = "folder"
nodeTypeUrl Graph     = "graph"
nodeTypeUrl Phylo     = "phylo"
nodeTypeUrl Individu  = "individu"
nodeTypeUrl Node      = "node"
nodeTypeUrl Nodes     = "nodes"
nodeTypeUrl NodeUser  = "user"
nodeTypeUrl NodeContact = "contact"
nodeTypeUrl Tree      = "tree"
nodeTypeUrl NodeList  = "lists"
nodeTypeUrl Texts     = "texts"
------------------------------------------------------------

type ListId = Int

data Path
  = Auth
  | Tab      TabType
  | Children NodeType Offset Limit (Maybe OrderBy)
  | GetNgrams
      { tabType        :: TabType
      , offset         :: Offset
      , limit          :: Limit
      , orderBy        :: Maybe OrderBy
      , listIds        :: Array ListId
      , termListFilter :: Maybe TermList
      , termSizeFilter :: Maybe TermSize
      , searchQuery    :: String
      }
  | PutNgrams TabType (Maybe ListId) (Maybe TermList)
  -- ^ The name is not good. In particular this URL is used both in PUT and POST.
  | NodeAPI NodeType
  | ListDocument (Maybe ListId)
  | Search  { {-id :: Int
            , query    :: Array String
            ,-}
              listId   :: Int
            , limit    :: Limit
            , offset   :: Offset
            , orderBy  :: Maybe OrderBy
            }
  | CorpusMetrics { tabType :: TabType
                  , listId  :: ListId
                  , limit   :: Maybe Limit
                  }
  | Chart { chartType :: ChartType
          , tabType   :: TabType
     --            , listId  :: ListId
      --           , limit   :: Maybe Limit
                 }

data ChartType = Histo | Scatter | ChartPie | ChartTree

instance showChartType :: Show ChartType
  where
    show Histo    = "chart"
    show Scatter  = "scatter"
    show ChartPie = "pie"
    show ChartTree = "tree"

data End = Back | Front | Static
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

