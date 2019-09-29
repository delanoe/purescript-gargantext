module Gargantext.Types where

import Prelude
import Data.Argonaut ( class DecodeJson, decodeJson, class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Either (Either(..))
import Prim.Row (class Union)
import URI.Query (Query)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data TermSize = MonoTerm | MultiTerm

data Term = Term String TermList

derive instance eqTermSize :: Eq TermSize

-- | Converts a data structure to a query string
class ToQuery a where
  toQuery :: a -> Query

instance showTermSize :: Show TermSize where
  show MonoTerm  = "MonoTerm"
  show MultiTerm = "MultiTerm"

readTermSize :: String -> Maybe TermSize
readTermSize "MonoTerm"  = Just MonoTerm
readTermSize "MultiTerm" = Just MultiTerm
readTermSize _           = Nothing

termSizes :: Array { desc :: String, mval :: Maybe TermSize }
termSizes = [ { desc: "All types",        mval: Nothing        }
            , { desc: "One-word terms",   mval: Just MonoTerm  }
            , { desc: "Multi-word terms", mval: Just MultiTerm }
            ]

data TermList = GraphTerm | StopTerm | CandidateTerm
-- TODO use generic JSON instance

derive instance eqTermList :: Eq TermList
derive instance ordTermList :: Ord TermList

instance encodeJsonTermList :: EncodeJson TermList where
  encodeJson GraphTerm     = encodeJson "GraphTerm"
  encodeJson StopTerm      = encodeJson "StopTerm"
  encodeJson CandidateTerm = encodeJson "CandidateTerm"

instance decodeJsonTermList :: DecodeJson TermList where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "GraphTerm"     -> pure GraphTerm
      "StopTerm"      -> pure StopTerm
      "CandidateTerm" -> pure CandidateTerm
      _               -> Left "Unexpected list name"

type ListTypeId = Int

listTypeId :: TermList -> ListTypeId
listTypeId GraphTerm     = 1
listTypeId StopTerm      = 2
listTypeId CandidateTerm = 3

instance showTermList :: Show TermList where
  show GraphTerm     = "GraphTerm"
  show StopTerm      = "StopTerm"
  show CandidateTerm = "CandidateTerm"

-- TODO: Can we replace the show instance above with this?
termListName :: TermList -> String
termListName GraphTerm = "Graph List"
termListName StopTerm = "Stop List"
termListName CandidateTerm = "Candidate List"

readTermList :: String -> Maybe TermList
readTermList "GraphTerm"     = Just GraphTerm
readTermList "StopTerm"      = Just StopTerm
readTermList "CandidateTerm" = Just CandidateTerm
readTermList _               = Nothing

termLists :: Array { desc :: String, mval :: Maybe TermList }
termLists = [ { desc: "All terms",   mval: Nothing      }
            , { desc: "Map terms",   mval: Just GraphTerm   }
            , { desc: "Stop terms",  mval: Just StopTerm  }
            , { desc: "Candidate terms", mval: Just CandidateTerm }
            ]

-- | Proof that row `r` is a subset of row `s`
class Optional (r :: # Type) (s :: # Type)
instance optionalInstance :: Union r t s => Optional r s

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

data NodePath = NodePath NodeType (Maybe Id)

nodePath :: NodePath -> String
nodePath (NodePath t i) = nodeTypePath t <> "/" <> id
  where id = maybe "" (\j -> "/" <> show j) i

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
