module Gargantext.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array as A
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.String as S
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect.Aff (Aff)
import Foreign as F
import Prim.Row (class Union)
import Reactix as R
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG
import URI.Query (Query)

import Gargantext.Prelude

data Handed = LeftHanded | RightHanded

switchHanded :: forall a. a -> a -> Handed -> a
switchHanded l _ LeftHanded = l
switchHanded _ r RightHanded = r

reverseHanded :: forall a. Handed -> Array a -> Array a
reverseHanded LeftHanded a = A.reverse a
reverseHanded RightHanded a = a

flipHanded :: R.Element -> R.Element -> Handed -> R.Element
flipHanded l r LeftHanded  = R.fragment [r, l]
flipHanded l r RightHanded = R.fragment [l, r]

derive instance Generic Handed _
instance Eq Handed where
  eq = genericEq


type ID      = Int
type Name    = String

newtype SessionId = SessionId String
type NodeID = Int

derive instance Generic SessionId _

instance Eq SessionId where
  eq = genericEq

instance Show SessionId where
  show (SessionId s) = s

data TermSize = MonoTerm | MultiTerm

data Term = Term String TermList

derive instance Eq TermSize

-- | Converts a data structure to a query string
class ToQuery a where
  toQuery :: a -> Query

instance Show TermSize where
  show MonoTerm  = "MonoTerm"
  show MultiTerm = "MultiTerm"

instance Read TermSize where
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

derive instance Eq TermList
derive instance Ord TermList

instance EncodeJson TermList where
  encodeJson MapTerm       = encodeJson "MapTerm"
  encodeJson StopTerm      = encodeJson "StopTerm"
  encodeJson CandidateTerm = encodeJson "CandidateTerm"

instance DecodeJson TermList where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "MapTerm"       -> pure MapTerm
      "StopTerm"      -> pure StopTerm
      "CandidateTerm" -> pure CandidateTerm
      s'              -> Left (AtKey s' $ TypeMismatch "Unexpected list name")

instance Show TermList where
  show MapTerm       = "MapTerm"
  show StopTerm      = "StopTerm"
  show CandidateTerm = "CandidateTerm"

-- TODO: Can we replace the show instance above with this?
termListName :: TermList -> String
termListName MapTerm = "Map List"
termListName StopTerm = "Stop List"
termListName CandidateTerm = "Candidate List"

instance Read TermList where
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
class Optional (r :: Row Type) (s :: Row Type)
instance Union r t s => Optional r s

showTabType' :: TabType -> String
showTabType' (TabCorpus   t) = show t
showTabType' (TabDocument t) = show t
showTabType' (TabPairing  t) = show t

data TabPostQuery = TabPostQuery {
    offset :: Int
  , limit :: Int
  , orderBy :: OrderBy
  , tabType :: TabType
  , query :: String
  }

instance EncodeJson TabPostQuery where
  encodeJson (TabPostQuery post) =
        "view"       := showTabType' post.tabType
     ~> "offset"     := post.offset
     ~> "limit"      := post.limit
     ~> "orderBy"    := show post.orderBy
     ~> "query"      := post.query
     ~> jsonEmptyObject

data NodeType = Annuaire
              | Corpus
              | Dashboard
              | Error
              | Folder
              | FolderPrivate
              | FolderPublic
              | FolderShared
              | Graph
              | Individu
              | Node
              | NodeContact
              | NodeList
              | NodeUser
              | Nodes
              | Phylo
              | Team
              | Texts
              | Tree
              | Url_Document
              -- TODO Optional Nodes
              | NodeFile
              | NodeFrameCalc
              | NodeFrameNotebook
              | NodeFrameWrite
              | NodeFrameVisio
              | NodePublic NodeType
derive instance Generic NodeType _
derive instance Eq NodeType
instance JSON.ReadForeign NodeType where
  readImpl f = do
    s <- F.readString f
    case read s of
      Nothing -> F.fail $ F.ErrorAtProperty s $ F.ForeignError "unknown property"
      Just nt -> pure nt
instance JSON.WriteForeign NodeType where writeImpl = JSON.writeImpl <<< show

instance Show NodeType where
  show NodeUser        = "NodeUser"

  show Folder          = "NodeFolder"
  show FolderPrivate   = "NodeFolderPrivate"  -- Node Private Worktop
  show FolderShared    = "NodeFolderShared"   -- Node Share Worktop
  show FolderPublic    = "NodeFolderPublic"   -- Node Public Worktop

  show Annuaire        = "NodeAnnuaire"
  show NodeContact     = "NodeContact"
  show Corpus          = "NodeCorpus"
  show Dashboard       = "NodeDashboard"
  show Url_Document    = "NodeDocument"
  show Error           = "NodeError"
  show Graph           = "NodeGraph"
  show Phylo           = "NodePhylo"
  show Individu        = "NodeIndividu"
  show Node            = "Node"
  show Nodes           = "Nodes"
  show Tree            = "NodeTree"
  show Team            = "NodeTeam"
  show NodeList        = "NodeList"
  show Texts           = "NodeDocs"
  show NodeFrameWrite  = "NodeFrameWrite"
  show NodeFrameCalc   = "NodeFrameCalc"
  show NodeFrameNotebook = "NodeFrameNotebook"
  show NodeFrameVisio    = "NodeFrameVisio"
  show (NodePublic nt) = "NodePublic" <> show nt
  show NodeFile        = "NodeFile"


instance Read NodeType where
  read "NodeUser"          = Just NodeUser
  read "NodeFolder"        = Just Folder
  read "NodeFolderPrivate" = Just FolderPrivate
  read "NodeFolderShared"  = Just FolderShared
  read "NodeFolderPublic"  = Just FolderPublic
  read "NodeAnnuaire"      = Just Annuaire
  read "NodeDashboard"     = Just Dashboard
  read "Document"          = Just Url_Document
  read "NodeGraph"         = Just Graph
  read "NodePhylo"         = Just Phylo
  read "Individu"          = Just Individu
  read "Node"              = Just Node
  read "Nodes"             = Just Nodes
  read "NodeCorpus"        = Just Corpus
  read "NodeContact"       = Just NodeContact
  read "Tree"              = Just Tree
  read "NodeTeam"          = Just Team
  read "NodeList"          = Just NodeList
  read "NodeTexts"         = Just Texts
  read "Annuaire"          = Just Annuaire
  read "NodeFrameWrite"    = Just NodeFrameWrite
  read "NodeFrameCalc"     = Just NodeFrameCalc
  read "NodeFrameNotebook"     = Just NodeFrameNotebook
  read "NodeFrameVisio"    = Just NodeFrameVisio
  read "NodeFile"          = Just NodeFile
  -- TODO NodePublic read ?
  read _                   = Nothing


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
fldr NodeFile _ = "fa fa-file"  -- TODO depending on mime type we can use fa-file-image etc

fldr Annuaire true  = "fa fa-address-card-o"
fldr Annuaire false = "fa fa-address-card"

fldr NodeContact true  = "fa fa-address-card-o"
fldr NodeContact false = "fa fa-address-card"

fldr NodeFrameWrite true  = "fa fa-file-text-o"
fldr NodeFrameWrite false = "fa fa-file-text"

fldr NodeFrameCalc true  = "fa fa-calculator"
fldr NodeFrameCalc false = "fa fa-calculator"

fldr NodeFrameNotebook true  = "fa fa-file-code-o"
fldr NodeFrameNotebook false = "fa fa-code"

fldr NodeFrameVisio true  = "fa fa-video-camera"
fldr NodeFrameVisio false = "fa fa-video-camera"



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
instance Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
-}
------------------------------------------------------------
instance DecodeJson NodeType where
  decodeJson json = do
    obj <- decodeJson json
    pure $ fromMaybe Error $ read obj

instance EncodeJson NodeType where
  encodeJson nodeType = encodeJson $ show nodeType

nodeTypePath :: NodeType -> String
nodeTypePath Folder          = "folder"
nodeTypePath FolderPrivate   = "folderPrivate"
nodeTypePath FolderShared    = "folderShared"
nodeTypePath FolderPublic    = "folderPublic"
nodeTypePath Annuaire        = "annuaire"
nodeTypePath Corpus          = "corpus"
nodeTypePath Dashboard       = "dashboard"
nodeTypePath Url_Document    = "document"
nodeTypePath Error           = "ErrorNodeType"
nodeTypePath Graph           = "graph"
nodeTypePath Phylo           = "phylo"
nodeTypePath Individu        = "individu"
nodeTypePath Node            = "node"
nodeTypePath Nodes           = "nodes"
nodeTypePath NodeUser        = "user"
nodeTypePath NodeContact     = "contact"
nodeTypePath Tree            = "tree"
nodeTypePath NodeList        = "lists"
nodeTypePath Texts           = "texts"
nodeTypePath Team            = "team"
nodeTypePath NodeFrameWrite  = "write"
nodeTypePath NodeFrameCalc   = "calc"
nodeTypePath NodeFrameNotebook = "code"
nodeTypePath NodeFrameVisio    = "visio"
nodeTypePath (NodePublic nt) = nodeTypePath nt
nodeTypePath NodeFile        = "file"

------------------------------------------------------------
type CorpusId   = Int
type DocId      = Int
type ListId     = Int
type AnnuaireId = Int
type ContactId  = Int

data ScoreType = Occurrences

derive instance Generic ScoreType _
instance Eq ScoreType where
  eq = genericEq
instance Show ScoreType where
  show = genericShow

type SearchQuery = String

type NgramsGetOpts =
  { limit          :: Limit
  , listIds        :: Array ListId
  , offset         :: Maybe Offset
  , orderBy        :: Maybe OrderBy
  , searchQuery    :: SearchQuery
  , tabType        :: TabType
  , termListFilter :: Maybe TermList
  , termSizeFilter :: Maybe TermSize
  }

type NgramsGetTableAllOpts =
  { listIds        :: Array ListId
  , tabType        :: TabType
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

instance Show ChartType
  where
    show Histo     = "chart"
    show Scatter   = "scatter"
    show ChartBar  = "bar"
    show ChartPie  = "pie"
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

derive instance Generic OrderBy _

instance Show OrderBy where
  show = genericShow

------------------------------------------------------------
-- V0 is the dummy case (impossible)
data ApiVersion = V0 | V10 | V11

derive instance Generic ApiVersion _
instance JSON.ReadForeign ApiVersion where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign ApiVersion where
  writeImpl V0 = F.unsafeToForeign $ JSON.writeImpl "v0"
  writeImpl V10 = F.unsafeToForeign $ JSON.writeImpl "v1.0"
  writeImpl V11 = F.unsafeToForeign $ JSON.writeImpl "v1.1"

instance Show ApiVersion where
  show V0  = "v0"
  show V10 = "v1.0"
  show V11 = "v1.1"

instance Eq ApiVersion where
  eq V10 V10 = true
  eq V11 V11 = true
  eq _ _ = false

instance EncodeJson ApiVersion where
  encodeJson v = encodeJson (show v)

instance DecodeJson ApiVersion where
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

derive instance Eq CTabNgramType
derive instance Ord CTabNgramType
instance Show CTabNgramType where
  show CTabTerms      = "Terms"
  show CTabSources    = "Sources"
  show CTabAuthors    = "Authors"
  show CTabInstitutes = "Institutes"
instance EncodeJson CTabNgramType where
  encodeJson t = encodeJson $ show t

data PTabNgramType = PTabPatents | PTabBooks | PTabCommunication

derive instance Eq PTabNgramType
derive instance Ord PTabNgramType
instance Show PTabNgramType where
  show PTabPatents       = "Patents"
  show PTabBooks         = "Books"
  show PTabCommunication = "Communication"
instance EncodeJson PTabNgramType where
  encodeJson t = encodeJson $ show t

data TabSubType a = TabDocs | TabNgramType a | TabTrash | TabMoreLikeFav | TabMoreLikeTrash

derive instance Eq a => Eq (TabSubType a)
derive instance Ord a => Ord (TabSubType a)
instance EncodeJson a => EncodeJson (TabSubType a) where
  encodeJson TabDocs =
       "type" := "TabDocs"
    ~> "data" := (Nothing :: Maybe String)
    ~> jsonEmptyObject
  encodeJson (TabNgramType a) =
       "type" := "TabNgramType"
    ~> "data" := encodeJson a
    ~> jsonEmptyObject
  encodeJson TabTrash =
       "type" := "TabTrash"
    ~> "data" := (Nothing :: Maybe String)
    ~> jsonEmptyObject
  encodeJson TabMoreLikeFav =
       "type" := "TabMoreLikeFav"
    ~> "data" := (Nothing :: Maybe String)
    ~> jsonEmptyObject
  encodeJson TabMoreLikeTrash =
       "type" := "TabMoreLikeTrash"
    ~> "data" := (Nothing :: Maybe String)
    ~> jsonEmptyObject
{-
instance DecodeJson a => DecodeJson (TabSubType a) where
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

instance Show a => Show (TabSubType a) where
  show TabDocs          = "Docs"
  show (TabNgramType a) = show a
  show TabTrash         = "Trash"
  show TabMoreLikeFav   = "MoreFav"
  show TabMoreLikeTrash = "MoreTrash"

data TabType
  = TabCorpus   (TabSubType CTabNgramType)
  | TabPairing  (TabSubType PTabNgramType)
  | TabDocument (TabSubType CTabNgramType)

derive instance Generic TabType _
derive instance Eq TabType
derive instance Ord TabType
instance Show TabType where
  show = genericShow
instance EncodeJson TabType where
  encodeJson (TabCorpus TabDocs)                         = encodeJson "Docs"
  encodeJson (TabCorpus (TabNgramType CTabAuthors))      = encodeJson "Authors"
  encodeJson (TabCorpus (TabNgramType CTabInstitutes))   = encodeJson "Institutes"
  encodeJson (TabCorpus (TabNgramType CTabSources))      = encodeJson "Sources"
  encodeJson (TabCorpus (TabNgramType CTabTerms))        = encodeJson "Terms"
  encodeJson (TabCorpus TabMoreLikeFav)                  = encodeJson "MoreFav"
  encodeJson (TabCorpus TabMoreLikeTrash)                = encodeJson "MoreTrash"
  encodeJson (TabCorpus TabTrash)                        = encodeJson "Trash"
  encodeJson (TabDocument TabDocs)                       = encodeJson "Docs"
  encodeJson (TabDocument (TabNgramType CTabAuthors))    = encodeJson "Authors"
  encodeJson (TabDocument (TabNgramType CTabInstitutes)) = encodeJson "Institutes"
  encodeJson (TabDocument (TabNgramType CTabSources))    = encodeJson "Sources"
  encodeJson (TabDocument (TabNgramType CTabTerms))      = encodeJson "Terms"
  encodeJson (TabDocument TabMoreLikeFav)                = encodeJson "MoreFav"
  encodeJson (TabDocument TabMoreLikeTrash)              = encodeJson "MoreTrash"
  encodeJson (TabDocument TabTrash)                      = encodeJson "Trash"
  encodeJson (TabPairing _d)                              = encodeJson "TabPairing"  -- TODO
-- ["Docs","Trash","MoreFav","MoreTrash","Terms","Sources","Authors","Institutes","Contacts"]
{-
instance DecodeJson TabType where
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

derive instance Generic Mode _
instance Show Mode where
  show = genericShow
derive instance Eq Mode
instance Ord Mode where
  compare = genericCompare
instance EncodeJson Mode where
  encodeJson x = encodeJson $ show x

modeTabType :: Mode -> CTabNgramType
modeTabType Authors    = CTabAuthors
modeTabType Institutes = CTabInstitutes
modeTabType Sources    = CTabSources
modeTabType Terms      = CTabTerms

modeFromString :: String -> Maybe Mode
modeFromString "Authors"    = Just Authors
modeFromString "Institutes" = Just Institutes
modeFromString "Sources"    = Just Sources
modeFromString "Terms"      = Just Terms
modeFromString _            = Nothing

-- Async tasks

-- corresponds to /add/form/async or /add/query/async
data AsyncTaskType = AddNode
                   | Form  -- this is file upload too
                   | GraphRecompute
                   | Query
                   | UpdateNgramsCharts
                   | UpdateNode
                   | UploadFile

derive instance Generic AsyncTaskType _
instance JSON.ReadForeign AsyncTaskType where
  readImpl = JSONG.enumSumRep
instance Eq AsyncTaskType where
  eq = genericEq
instance Show AsyncTaskType where
  show = genericShow

asyncTaskTypePath :: AsyncTaskType -> String
asyncTaskTypePath AddNode            = "async/nobody/"
asyncTaskTypePath Form               = "add/form/async/"
asyncTaskTypePath GraphRecompute     = "async/recompute/"
asyncTaskTypePath Query              = "query/"
asyncTaskTypePath UpdateNgramsCharts = "ngrams/async/charts/update/"
asyncTaskTypePath UpdateNode         = "update/"
asyncTaskTypePath UploadFile         = "async/file/add/"


type AsyncTaskID = String

data AsyncTaskStatus = IsRunning
                     | IsPending
                     | IsReceived
                     | IsStarted
                     | IsFailure
                     | IsFinished
                     | IsKilled
derive instance Generic AsyncTaskStatus _
instance JSON.ReadForeign AsyncTaskStatus where
  readImpl = JSONG.enumSumRep
instance Show AsyncTaskStatus where
  show = genericShow
derive instance Eq AsyncTaskStatus
-- instance Read AsyncTaskStatus where
--   read "IsFailure"  = Just Failed
--   read "IsFinished" = Just Finished
--   read "IsKilled"   = Just Killed
--   read "IsPending"  = Just Pending
--   read "IsReceived" = Just Received
--   read "IsRunning"  = Just Running
--   read "IsStarted"  = Just Started
--   read _            = Nothing

newtype AsyncTask =
  AsyncTask { id     :: AsyncTaskID
            , status :: AsyncTaskStatus
            }

derive instance Generic AsyncTask _
derive instance Newtype AsyncTask _
derive newtype instance JSON.ReadForeign AsyncTask
instance Eq AsyncTask where
  eq = genericEq

newtype AsyncTaskWithType = AsyncTaskWithType {
    task :: AsyncTask
  , typ  :: AsyncTaskType
  }
derive instance Generic AsyncTaskWithType _
derive instance Newtype AsyncTaskWithType _
derive newtype instance JSON.ReadForeign AsyncTaskWithType
instance Eq AsyncTaskWithType where
  eq = genericEq

newtype AsyncProgress = AsyncProgress {
    id :: AsyncTaskID
  , log :: Array AsyncTaskLog
  , status :: AsyncTaskStatus
  }
derive instance Generic AsyncProgress _
derive instance Newtype AsyncProgress _
derive newtype instance JSON.ReadForeign AsyncProgress

newtype AsyncTaskLog = AsyncTaskLog {
    events :: Array String
  , failed :: Int
  , remaining :: Int
  , succeeded :: Int
  }
derive instance Generic AsyncTaskLog _
derive instance Newtype AsyncTaskLog _
derive newtype instance JSON.ReadForeign AsyncTaskLog

progressPercent :: AsyncProgress -> Number
progressPercent (AsyncProgress {log}) = perc
  where
    perc = case A.head log of
      Nothing -> 0.0
      Just (AsyncTaskLog {failed, remaining, succeeded}) -> 100.0*nom/denom
        where
          nom = toNumber $ failed + succeeded
          denom = toNumber $ failed + succeeded + remaining

---------------------------------------------------------------------------
-- | GarganText Internal Sugar

prettyNodeType :: NodeType -> String
prettyNodeType nt = S.replace (S.Pattern "Node")   (S.Replacement " ")
                  $ S.replace (S.Pattern "Folder") (S.Replacement " ")
                  $ show nt

---------------------------------------------------------------------------

data SidePanelState = InitialClosed | Opened | Closed
derive instance Generic SidePanelState _
instance Eq SidePanelState where
  eq = genericEq

toggleSidePanelState :: SidePanelState -> SidePanelState
toggleSidePanelState InitialClosed = Opened
toggleSidePanelState Closed        = Opened
toggleSidePanelState Opened        = Closed
