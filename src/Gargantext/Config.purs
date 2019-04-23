{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage (depending on your Config):
toUrl Back  Corpus 1 == "http://localhost:8008/api/v1.0/corpus/1"
toUrl Front Corpus 1 == "http://localhost:2015/#/corpus/1"
-}
module Gargantext.Config where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Gargantext.Types

endConfig :: EndConfig
endConfig = endConfig' V10

endConfig' :: ApiVersion -> EndConfig
endConfig' v = { front : frontRelative
               , back  : backDemo v  }
--               , back  : backDemo v  }

------------------------------------------------------------------------
frontRelative :: Config
frontRelative = { baseUrl: ""
                , prePath: "/#/"
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

endOf :: forall cfg. End -> { front :: cfg, back :: cfg } -> cfg
endOf Back  = _.back
endOf Front = _.front

endBaseUrl :: End -> EndConfig -> UrlBase
endBaseUrl end c = (endOf end c).baseUrl

endPathUrl :: End -> EndConfig -> Path -> Maybe Id -> UrlPath
endPathUrl end = pathUrl <<< endOf end

tabTypeDocs :: TabType -> UrlPath
tabTypeDocs (TabCorpus  t) = "table?view="   <> show t
tabTypeDocs (TabPairing t) = "pairing?view=" <> show t

limitUrl :: Limit -> UrlPath
limitUrl l = "&limit=" <> show l

offsetUrl :: Offset -> UrlPath
offsetUrl o = "&offset=" <> show o

orderUrl :: forall a. Show a => Maybe a -> UrlPath
orderUrl = maybe "" (\x -> "&order=" <> show x)

showTabType' :: TabType -> String
showTabType' (TabCorpus  t) = show t
showTabType' (TabPairing t) = show t

tabTypeNgramsGet :: TabType -> UrlPath
tabTypeNgramsGet (TabCorpus  t) = "listGet?ngramsType=" <> show t
tabTypeNgramsGet (TabPairing t) = "listGet?ngramsType=" <> show t -- TODO

tabTypeNgramsPut :: TabType -> UrlPath
tabTypeNgramsPut (TabCorpus  t) = "list?ngramsType=" <> show t
tabTypeNgramsPut (TabPairing t) = "list?ngramsType=" <> show t -- TODO

pathUrl :: Config -> Path -> Maybe Id -> UrlPath
pathUrl c (Tab t o l s) i =
    pathUrl c (NodeAPI Node) i <>
      "/" <> tabTypeDocs t <> offsetUrl o <> limitUrl l <> orderUrl s
pathUrl c (Children n o l s) i =
    pathUrl c (NodeAPI Node) i <>
      "/" <> "children?type=" <> show n <> offsetUrl o <> limitUrl l <> orderUrl s
pathUrl c (GetNgrams
            { tabType: t
            , offset: o
            , limit: l
            , listIds
            , termListFilter: tlf
            , termSizeFilter: tsf
            , searchQuery: q
            }) i =
    pathUrl c (NodeAPI Node) i <> "/" <> tabTypeNgramsGet t
      <> offsetUrl o <> limitUrl l
      <> foldMap (\x -> "&list=" <> show x) listIds
      <> foldMap (\x -> "&listType=" <> show x) tlf
      <> foldMap (\x -> case x of
                          MonoTerm  -> "&minTermSize=0&maxTermSize=1"
                          MultiTerm -> "&minTermSize=2"
                 ) tsf
      <> if q == "" then "" else ("&search=" <> q)
pathUrl c (PutNgrams t listid) i =
    pathUrl c (NodeAPI Node) i <> "/" <> tabTypeNgramsPut t <> listid'
  where
    listid' = maybe "" (\x -> "&list=" <> show x) listid
pathUrl c Auth Nothing = c.prePath <> "auth"
pathUrl c Auth (Just _) = "impossible" -- TODO better types
pathUrl c (NodeAPI nt) i = c.prePath <> nodeTypeUrl nt <> (maybe "" (\i' -> "/" <> show i') i)
pathUrl c (Search {limit,offset,orderBy}) _TODO =
  c.prePath <> "search/?dummy=dummy"
    <> offsetUrl offset <> limitUrl limit <> orderUrl orderBy
pathUrl c (CorpusMetrics {tabType, listId, limit}) i =
    pathUrl c (NodeAPI Corpus) i <> "/metrics"
      <> "?list=" <> show listId
      <> "&ngramsType=" <> showTabType' tabType
      <> maybe "" (\x -> "&limit=" <> show x) limit
-- TODO fix this url path
pathUrl c (Chart {chartType, tabType}) i =
    pathUrl c (NodeAPI Corpus) i <> "/" <> show chartType
      <> "?ngramsType=" <> showTabType' tabType
      <> "&listType=GraphTerm" -- <> show listId
      -- <> maybe "" (\x -> "&limit=" <> show x) limit


------------------------------------------------------------

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
--                | NodeDocument
              | CorpusV3
              | Dashboard
              | Url_Document
              | Error
              | Folder
              | Graph
              | Individu
              | Node
              | Nodes
              | Tree
              | NodeList


instance showNodeType :: Show NodeType where
  show NodeUser      = "NodeUser"
  show Annuaire      = "Annuaire"
  show NodeContact   = "NodeContact"
  show Corpus        = "NodeCorpus"
  show CorpusV3      = "NodeCorpusV3"
  show Dashboard     = "NodeDashboard"
  show Url_Document  = "NodeDocument"
  --show NodeDocument  = "NodeDocument"
  show Error         = "NodeError"
  show Folder        = "NodeFolder"
  show Graph         = "NodeGraph"
  show Individu      = "NodeIndividu"
  show Node          = "Node"
  show Nodes         = "Nodes"
  show Tree          = "NodeTree"
  show NodeList      = "NodeList"

type ListId = Int

data Path
  = Auth
  | Tab      TabType  Offset Limit (Maybe OrderBy)
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
  | PutNgrams TabType (Maybe ListId)
  | NodeAPI NodeType
  | Search  { {-id :: Int
            , query    :: Array String
            ,-} limit  :: Limit
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

data End = Back | Front
type Id  = Int

type Limit  = Int
type Offset = Int
data OrderBy = DateAsc  | DateDesc
             | TitleAsc | TitleDesc
             | ScoreDesc  | ScoreAsc

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

data TabSubType a = TabDocs | TabNgramType a | TabTrash

derive instance eqTabSubType :: Eq a => Eq (TabSubType a)

instance showTabSubType :: Show a => Show (TabSubType a) where
  show TabDocs          = "Docs"
  show (TabNgramType a) = show a
  show TabTrash         = "Trash"

data TabType
  = TabCorpus  (TabSubType CTabNgramType)
  | TabPairing (TabSubType PTabNgramType)

derive instance eqTabType :: Eq TabType

derive instance genericTabType :: Generic TabType _

instance showTabType :: Show TabType where
  show = genericShow

------------------------------------------------------------
nodeTypeUrl :: NodeType -> Url
nodeTypeUrl Annuaire  = "annuaire"
nodeTypeUrl Corpus    = "corpus"
nodeTypeUrl CorpusV3  = "corpus"
nodeTypeUrl Dashboard = "dashboard"
nodeTypeUrl Url_Document  = "document"
nodeTypeUrl Error     = "ErrorNodeType"
nodeTypeUrl Folder    = "folder"
nodeTypeUrl Graph     = "graph"
nodeTypeUrl Individu  = "individu"
nodeTypeUrl Node      = "node"
nodeTypeUrl Nodes      = "nodes"
nodeTypeUrl NodeUser  = "user"
nodeTypeUrl NodeContact = "contact"
nodeTypeUrl Tree      = "tree"
nodeTypeUrl NodeList  = "list"

readNodeType :: String -> NodeType
readNodeType "NodeAnnuaire"  = Annuaire
readNodeType "NodeDashboard" = Dashboard
readNodeType "Document"      = Url_Document
readNodeType "NodeFolder"    = Folder
readNodeType "NodeGraph"     = Graph
readNodeType "Individu"      = Individu
readNodeType "Node"          = Node
readNodeType "Nodes"         = Nodes
readNodeType "NodeCorpus"    = Corpus
readNodeType "NodeCorpusV3"  = CorpusV3
readNodeType "NodeUser"      = NodeUser
readNodeType "NodeContact"   = NodeContact
readNodeType "Tree"          = Tree
readNodeType "NodeList"      = NodeList
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
