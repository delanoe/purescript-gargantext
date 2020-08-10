module Gargantext.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect.Aff (Aff)
import Gargantext.Prelude (class Read, read, class Show, show)
import Prelude
import Prim.Row (class Union)
import URI.Query (Query)

type ID     = Int
type Name   = String
type Reload = Int

newtype SessionId = SessionId String
type NodeID = Int

derive instance genericSessionId :: Generic SessionId _

instance eqSessionId :: Eq SessionId where
  eq = genericEq

instance showSessionId :: Show SessionId where
  show (SessionId s) = s

data TermSize = MonoTerm | MultiTerm

data Term = Term String TermList

derive instance eqTermSize :: Eq TermSize

-- | Converts a data structure to a query string
class ToQuery a where
  toQuery :: a -> Query

instance showTermSize :: Show TermSize where
  show MonoTerm  = "MonoTerm"
  show MultiTerm = "MultiTerm"

instance readTermSize :: Read TermSize where
  read :: String -> Maybe TermSize
  read "MonoTerm"  = Just MonoTerm
  read "MultiTerm" = Just MultiTerm
  read _           = Nothing

termSizes :: Array { desc :: String, mval :: Maybe TermSize }
termSizes = [ { desc: "All types",        mval: Nothing        }
            , { desc: "One-word terms",   mval: Just MonoTerm  }
            , { desc: "Multi-word terms", mval: Just MultiTerm }
            ]

data TermList = MapTerm | StopTerm | CandidateTerm
-- TODO use generic JSON instance

derive instance eqTermList :: Eq TermList
derive instance ordTermList :: Ord TermList

instance encodeJsonTermList :: EncodeJson TermList where
  encodeJson MapTerm       = encodeJson "MapTerm"
  encodeJson StopTerm      = encodeJson "StopTerm"
  encodeJson CandidateTerm = encodeJson "CandidateTerm"

instance decodeJsonTermList :: DecodeJson TermList where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "MapTerm"       -> pure MapTerm
      "StopTerm"      -> pure StopTerm
      "CandidateTerm" -> pure CandidateTerm
      _               -> Left "Unexpected list name"

type ListTypeId = Int

listTypeId :: TermList -> ListTypeId
listTypeId MapTerm     = 1
listTypeId StopTerm      = 2
listTypeId CandidateTerm = 3

instance showTermList :: Show TermList where
  show MapTerm       = "MapTerm"
  show StopTerm      = "StopTerm"
  show CandidateTerm = "CandidateTerm"

-- TODO: Can we replace the show instance above with this?
termListName :: TermList -> String
termListName MapTerm = "Map List"
termListName StopTerm = "Stop List"
termListName CandidateTerm = "Candidate List"

instance readTermList :: Read TermList where
  read :: String -> Maybe TermList
  read "MapTerm"     = Just MapTerm
  read "StopTerm"      = Just StopTerm
  read "CandidateTerm" = Just CandidateTerm
  read _               = Nothing

termLists :: Array { desc :: String, mval :: Maybe TermList }
termLists = [ { desc: "All terms",   mval: Nothing      }
            , { desc: "Map terms",   mval: Just MapTerm   }
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
              | Folder | FolderPrivate | FolderShared | FolderPublic
              | Annuaire
              | NodeContact
              | Corpus
              | Team
              | Url_Document
              | Dashboard
              | Error
              | Graph
              | Phylo
              | Individu
              | Node
              | Nodes
              | Tree
              | NodeList
              | Texts
              -- TODO Optional Nodes
              | NodeFrameWrite
              | NodeFrameCalc
              | NodePublic NodeType


derive instance eqNodeType :: Eq NodeType

instance showNodeType :: Show NodeType where
  show NodeUser      = "NodeUser"

  show Folder        = "NodeFolder"
  show FolderPrivate = "NodeFolderPrivate"  -- Node Private Worktop
  show FolderShared  = "NodeFolderShared"   -- Node Share Worktop
  show FolderPublic  = "NodeFolderPublic"   -- Node Public Worktop

  show Annuaire      = "NodeAnnuaire"
  show NodeContact   = "NodeContact"
  show Corpus        = "NodeCorpus"
  show Dashboard     = "NodeDashboard"
  show Url_Document  = "NodeDocument"
  show Error         = "NodeError"
  show Graph         = "NodeGraph"
  show Phylo         = "NodePhylo"
  show Individu      = "NodeIndividu"
  show Node          = "Node"
  show Nodes         = "Nodes"
  show Tree          = "NodeTree"
  show Team          = "NodeTeam"
  show NodeList      = "NodeList"
  show Texts         = "NodeTexts"
  show NodeFrameWrite = "NodeFrameWrite"
  show NodeFrameCalc  = "NodeFrameCalc"
  show (NodePublic nt) = "NodePublic" <> show nt


instance readNodeType :: Read NodeType where
  read "NodeUser"          = Just NodeUser
  read "NodeFolder"        = Just Folder
  read "NodeFolderPrivate" = Just FolderPrivate
  read "NodeFolderShared"  = Just FolderShared
  read "NodeFolderPublic"  = Just FolderPublic
  read "NodeAnnuaire"  = Just Annuaire
  read "NodeDashboard" = Just Dashboard
  read "Document"      = Just Url_Document
  read "NodeGraph"     = Just Graph
  read "NodePhylo"     = Just Phylo
  read "Individu"      = Just Individu
  read "Node"          = Just Node
  read "Nodes"         = Just Nodes
  read "NodeCorpus"    = Just Corpus
  read "NodeContact"   = Just NodeContact
  read "Tree"          = Just Tree
  read "NodeTeam"      = Just Team
  read "NodeList"      = Just NodeList
  read "NodeTexts"     = Just Texts
  read "Annuaire"      = Just Annuaire
  read "NodeFrameWrite" = Just NodeFrameWrite
  read "NodeFrameCalc"  = Just NodeFrameCalc
  -- TODO NodePublic read ?
  read _               = Nothing


fldr :: NodeType -> Boolean -> String
fldr NodeUser false = "fa fa-user-circle"
fldr NodeUser true  = "fa fa-user"
------------------------------------------------------
fldr Folder  false  = "fa fa-folder"
fldr Folder  true   = "fa fa-folder-open-o"
------------------------------------------------------
fldr FolderPrivate true  = "fa fa-lock"
fldr FolderPrivate false = "fa fa-lock-circle"

fldr FolderShared  true  = "fa fa-share-alt"
fldr FolderShared  false = "fa fa-share-circle"
fldr Team  true   = "fa fa-users"
fldr Team  false  = "fa fa-users-closed"

fldr FolderPublic true  = "fa fa-globe-circle"
fldr FolderPublic false = "fa fa-globe"
------------------------------------------------------

fldr Corpus true  = "fa fa-book"
fldr Corpus false = "fa fa-book-circle"

fldr Phylo _ = "fa fa-code-fork"

fldr Graph _ = "fa fa-hubzilla"
fldr Texts _ = "fa fa-newspaper-o"
fldr Dashboard _ = "fa fa-signal"
fldr NodeList _ = "fa fa-list"

fldr Annuaire true  = "fa fa-address-card-o"
fldr Annuaire false = "fa fa-address-card"

fldr NodeContact true  = "fa fa-address-card-o"
fldr NodeContact false = "fa fa-address-card"

fldr NodeFrameWrite true  = "fa fa-file-text-o"
fldr NodeFrameWrite false = "fa fa-file-text"

fldr NodeFrameCalc true  = "fa fa-calculator"
fldr NodeFrameCalc false = "fa fa-calculator"

fldr (NodePublic nt) b   = fldr nt b

fldr _        true   = "fa fa-folder-open"
fldr _        false  = "fa fa-folder-o"


publicize :: NodeType -> NodeType
publicize (NodePublic nt) = NodePublic nt
publicize nt              = NodePublic nt

isPublic :: NodeType -> Boolean
isPublic (NodePublic _) = true
isPublic FolderPublic   = true
isPublic _              = false

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
    pure $ fromMaybe Error $ read obj

instance encodeJsonNodeType :: EncodeJson NodeType where
  encodeJson nodeType = encodeJson $ show nodeType

nodeTypePath :: NodeType -> String
nodeTypePath Folder    = "folder"
nodeTypePath FolderPrivate = "folderPrivate"
nodeTypePath FolderShared  = "folderShared"
nodeTypePath FolderPublic  = "folderPublic"
nodeTypePath Annuaire  = "annuaire"
nodeTypePath Corpus    = "corpus"
nodeTypePath Dashboard = "dashboard"
nodeTypePath Url_Document  = "document"
nodeTypePath Error     = "ErrorNodeType"
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
nodeTypePath Team      = "team"
nodeTypePath NodeFrameWrite = "write"
nodeTypePath NodeFrameCalc  = "calc"
nodeTypePath (NodePublic nt) = nodeTypePath nt

------------------------------------------------------------

type ListId = Int

data ScoreType = Occurrences

derive instance genericScoreType :: Generic ScoreType _
instance eqScoreType :: Eq ScoreType where
  eq = genericEq
instance showScoreType :: Show ScoreType where
  show = genericShow

type SearchQuery = String

type NgramsGetOpts =
  { tabType        :: TabType
  , limit          :: Limit
  , offset         :: Maybe Offset
  , orderBy        :: Maybe OrderBy
  , listIds        :: Array ListId
  , termListFilter :: Maybe TermList
  , termSizeFilter :: Maybe TermSize
  , searchQuery    :: SearchQuery
  }

type NgramsGetTableAllOpts =
  { tabType        :: TabType
  , listIds        :: Array ListId
  , scoreType      :: ScoreType
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
  , limit     :: Maybe Limit
  , listId    :: Maybe ListId
  , tabType   :: TabType
  }

data NodePath = NodePath SessionId NodeType (Maybe Id)

nodePath :: NodePath -> String
nodePath (NodePath s t i) = nodeTypePath t <> "/" <> show s <> id
  where id = maybe "" (\j -> "/" <> show j) i

data ChartType = Histo | Scatter | ChartPie | ChartBar | ChartTree

instance showChartType :: Show ChartType
  where
    show Histo    = "chart"
    show Scatter  = "scatter"
    show ChartBar = "bar"
    show ChartPie = "pie"
    show ChartTree = "tree"

chartTypeFromString :: String -> Maybe ChartType
chartTypeFromString "bar"     = Just ChartBar
chartTypeFromString "chart"   = Just Histo
chartTypeFromString "pie"     = Just ChartPie
chartTypeFromString "scatter" = Just Scatter
chartTypeFromString "tree"    = Just ChartTree
chartTypeFromString _         = Nothing

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
-- V0 is the dummy case (impossible)
data ApiVersion = V0 | V10 | V11

instance showApiVersion :: Show ApiVersion where
  show V0  = "v0"
  show V10 = "v1.0"
  show V11 = "v1.1"

instance eqApiVersion :: Eq ApiVersion where
  eq V10 V10 = true
  eq V11 V11 = true
  eq _ _ = false

instance encodeJsonApiVersion :: EncodeJson ApiVersion where
  encodeJson v = encodeJson (show v)

instance decodeJsonApiVersion :: DecodeJson ApiVersion where
  decodeJson json = do
    v <- decodeJson json
    case v of
         "v1.0" -> pure V10
         "v1.1" -> pure V11
         _      -> pure V0
------------------------------------------------------------

-- Types of ngrams. Used to display user-selectable tabs and is sent via API,
-- wrapped in `TabNgramType a :: TabSubType`
data CTabNgramType = CTabTerms | CTabSources | CTabAuthors | CTabInstitutes

derive instance eqCTabNgramType :: Eq CTabNgramType
derive instance ordCTabNgramType :: Ord CTabNgramType

instance showCTabNgramType :: Show CTabNgramType where
  show CTabTerms      = "Terms"
  show CTabSources    = "Sources"
  show CTabAuthors    = "Authors"
  show CTabInstitutes = "Institutes"

data PTabNgramType = PTabPatents | PTabBooks | PTabCommunication

derive instance eqPTabNgramType :: Eq PTabNgramType
derive instance ordPTabNgramType :: Ord PTabNgramType

instance showPTabNgramType :: Show PTabNgramType where
  show PTabPatents       = "Patents"
  show PTabBooks         = "Books"
  show PTabCommunication = "Communication"

data TabSubType a = TabDocs | TabNgramType a | TabTrash | TabMoreLikeFav | TabMoreLikeTrash

derive instance eqTabSubType :: Eq a => Eq (TabSubType a)
derive instance ordTabSubType :: Ord a => Ord (TabSubType a)
{- instance encodeTabSubType a :: EncodeJson a => EncodeJson (TabSubType a) where
  encodeJson TabDocs =
       "type" := "TabDocs"
    ~> "data" := Nothing
    ~> jsonEmptyObject
  encodeJson (TabNgramType a) =
       "type" := "TabNgramType"
    ~> "data" := encodeJson a
    ~> jsonEmptyObject
  encodeJson TabTrash =
       "type" := "TabTrash"
    ~> "data" := Nothing
    ~> jsonEmptyObject
  encodeJson TabMoreLikeFav =
       "type" := "TabMoreLikeFav"
    ~> "data" := Nothing
    ~> jsonEmptyObject
  encodeJson TabMoreLikeTrash =
       "type" := "TabMoreLikeTrash"
    ~> "data" := Nothing
    ~> jsonEmptyObject
instance decodeTabSubType a :: DecodeJson a => DecodeJson (TabSubType a) where
  decodeJson j = do
    obj <- decodeJson j
    typ <- obj .: "type"
    dat <- obj .: "data"
    case typ of
      "TabDocs" -> TabDocs
      "TabNgramType" -> TabNgramType dat
      "TabTrash" -> TabTrash
      "TabMoreLikeFav" -> TabMoreLikeFav
      "TabMoreLikeTrash" -> TabMoreLikeTrash
      _ -> Left ("Unknown type '" <> typ <> "'") -}

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

derive instance genericTabType :: Generic TabType _
derive instance eqTabType :: Eq TabType
derive instance ordTabType :: Ord TabType
instance showTabType :: Show TabType where
  show = genericShow
{- instance encodeTabType :: EncodeJson TabType where
  encodeJson (TabCorpus d) = 
       "type"  := "TabCorpus"
    ~> "data"  := encodeJson d
    ~> jsonEmptyObject
  encodeJson (TabDocument d) = 
       "type"  := "TabDocument"
    ~> "data"  := encodeJson d
    ~> jsonEmptyObject
  encodeJson (TabPairing d) = 
       "type"  := "TabPairing"
    ~> "data"  := encodeJson d
    ~> jsonEmptyObject
instance decodeTabType :: DecodeJson TabType where
  decodeJson j = do
    obj <- decodeJson j
    typ <- obj .: "type"
    dat <- obj .: "data"
    case typ of
      "TabCorpus" -> TabCorpus dat
      "TabDocument" -> TabDocument dat
      "TabPairing" -> TabPairing dat
      _ -> Left ("Unknown type '" <> typ <> "'") -}

type TableResult a = {count :: Int, docs :: Array a}
type AffTableResult a = Aff (TableResult a)

data Mode = Authors
          | Sources
          | Institutes
          | Terms

derive instance genericMode :: Generic Mode _
instance showMode :: Show Mode where
  show = genericShow
derive instance eqMode :: Eq Mode
instance ordMode :: Ord Mode where
  compare = genericCompare
instance encodeMode :: EncodeJson Mode where
  encodeJson x = encodeJson $ show x

modeTabType :: Mode -> CTabNgramType
modeTabType Authors    = CTabAuthors
modeTabType Sources    = CTabSources
modeTabType Institutes = CTabInstitutes
modeTabType Terms      = CTabTerms

modeFromString :: String -> Maybe Mode
modeFromString "Authors" = Just Authors
modeFromString "Sources" = Just Sources
modeFromString "Institutes" = Just Institutes
modeFromString "Terms" = Just Terms
modeFromString _ = Nothing

-- Async tasks

-- corresponds to /add/form/async or /add/query/async
data AsyncTaskType = Form
                   | UploadFile
                   | GraphT
                   | Query
                   | AddNode
                   | UpdateNode

derive instance genericAsyncTaskType :: Generic AsyncTaskType _
instance eqAsyncTaskType :: Eq AsyncTaskType where
  eq = genericEq
instance showAsyncTaskType :: Show AsyncTaskType where
  show = genericShow
instance encodeJsonAsyncTaskType :: EncodeJson AsyncTaskType where
  encodeJson t     = encodeJson $ show t
instance decodeJsonAsyncTaskType :: DecodeJson AsyncTaskType where
  decodeJson json = do
    obj <- decodeJson json
    case obj of
      "Form"       -> pure Form
      "UploadFile" -> pure UploadFile
      "GraphT"     -> pure GraphT
      "Query"      -> pure Query
      "AddNode"    -> pure AddNode
      s            -> Left ("Unknown string " <> s)

asyncTaskTypePath :: AsyncTaskType -> String
asyncTaskTypePath Form       = "add/form/async/"
asyncTaskTypePath UploadFile = "add/file/async/"
asyncTaskTypePath Query      = "query/"
asyncTaskTypePath GraphT     = "async/"
asyncTaskTypePath AddNode    = "async/nobody/"
asyncTaskTypePath UpdateNode = "update/"


type AsyncTaskID = String

data AsyncTaskStatus = Running
                     | Pending
                     | Received
                     | Started
                     | Failed
                     | Finished
                     | Killed
derive instance genericAsyncTaskStatus :: Generic AsyncTaskStatus _

instance showAsyncTaskStatus :: Show AsyncTaskStatus where
  show = genericShow
derive instance eqAsyncTaskStatus :: Eq AsyncTaskStatus

instance encodeJsonAsyncTaskStatus :: EncodeJson AsyncTaskStatus where
  encodeJson s = encodeJson $ show s

instance decodeJsonAsyncTaskStatus :: DecodeJson AsyncTaskStatus where
  decodeJson json = do
    obj <- decodeJson json
    pure $ fromMaybe Running $ read obj

instance readAsyncTaskStatus :: Read AsyncTaskStatus where
  read "IsFailure"  = Just Failed
  read "IsFinished" = Just Finished
  read "IsKilled"   = Just Killed
  read "IsPending"  = Just Pending
  read "IsReceived" = Just Received
  read "IsRunning"  = Just Running
  read "IsStarted"  = Just Started
  read _            = Nothing

newtype AsyncTask =
  AsyncTask { id     :: AsyncTaskID
            , status :: AsyncTaskStatus
            }

derive instance genericAsyncTask :: Generic AsyncTask _
instance eqAsyncTask :: Eq AsyncTask where
  eq = genericEq
instance encodeJsonAsyncTask :: EncodeJson AsyncTask where
  encodeJson (AsyncTask { id, status }) =
        "id"       := id
     ~> "status"   := status
     ~> jsonEmptyObject
instance decodeJsonAsyncTask :: DecodeJson AsyncTask where
  decodeJson json = do
    obj    <- decodeJson json
    id     <- obj .: "id"
    status <- obj .: "status"
    pure $ AsyncTask { id, status }

newtype AsyncTaskWithType = AsyncTaskWithType {
    task :: AsyncTask
  , typ  :: AsyncTaskType
  }
derive instance genericAsyncTaskWithType :: Generic AsyncTaskWithType _
instance eqAsyncTaskWithType :: Eq AsyncTaskWithType where
  eq = genericEq
instance encodeJsonAsyncTaskWithType :: EncodeJson AsyncTaskWithType where
  encodeJson (AsyncTaskWithType { task, typ }) =
        "task"       := task
     ~> "typ"        := typ
     ~> jsonEmptyObject
instance decodeJsonAsyncTaskWithType :: DecodeJson AsyncTaskWithType where
  decodeJson json = do
    obj  <- decodeJson json
    task <- obj .: "task"
    typ  <- obj .: "typ"
    pure $ AsyncTaskWithType { task, typ }

newtype AsyncProgress = AsyncProgress {
    id :: AsyncTaskID
  , log :: Array AsyncTaskLog
  , status :: AsyncTaskStatus
  }
derive instance genericAsyncProgress :: Generic AsyncProgress _
instance decodeJsonAsyncProgress :: DecodeJson AsyncProgress where
  decodeJson json = do
    obj    <- decodeJson json
    id     <- obj .: "id"
    log    <- obj .: "log"
    status <- obj .: "status"
    pure $ AsyncProgress {id, log, status}

newtype AsyncTaskLog = AsyncTaskLog {
    events :: Array String
  , failed :: Int
  , remaining :: Int
  , succeeded :: Int
  }
derive instance genericAsyncTaskLog :: Generic AsyncTaskLog _
instance decodeJsonAsyncTaskLog :: DecodeJson AsyncTaskLog where
  decodeJson json = do
    obj       <- decodeJson json
    events    <- obj .: "events"
    failed    <- obj .: "failed"
    remaining <- obj .: "remaining"
    succeeded <- obj .: "succeeded"
    pure $ AsyncTaskLog {events, failed, remaining, succeeded}

progressPercent :: AsyncProgress -> Number
progressPercent (AsyncProgress {log}) = perc
  where
    perc = case A.head log of
      Nothing -> 0.0
      Just (AsyncTaskLog {failed, remaining, succeeded}) -> 100.0*nom/denom
        where
          nom = toNumber $ failed + succeeded
          denom = toNumber $ failed + succeeded + remaining
