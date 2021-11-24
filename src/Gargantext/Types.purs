module Gargantext.Types where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String as S
import Effect.Aff (Aff)
import Foreign as F
import Gargantext.Components.Lang (class Translate, Lang(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Utils.Glyphicon (classNamePrefix, glyphiconToCharCode)
import GraphQL.Client.Args (class ArgGql)
import GraphQL.Client.Variables.TypeName (class VarTypeName)
import Prim.Row (class Union)
import Reactix as R
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG
import URI.Query (Query)

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
derive instance Generic TermSize _
instance Eq TermSize where eq = genericEq

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
derive instance Generic TermList _
instance Eq TermList where eq = genericEq
instance Ord TermList where compare = genericCompare
instance JSON.WriteForeign TermList where writeImpl = JSON.writeImpl <<< show
instance JSON.ReadForeign TermList where readImpl = JSONG.enumSumRep
instance Show TermList where show = genericShow

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

newtype TabPostQuery = TabPostQuery {
    offset :: Int
  , limit :: Int
  , orderBy :: OrderBy
  , tabType :: TabType
  , query :: String
  }
derive instance Generic TabPostQuery _
derive instance Newtype TabPostQuery _
derive newtype instance JSON.WriteForeign TabPostQuery

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
              | NodeTexts
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
instance ArgGql NodeType NodeType
instance VarTypeName NodeType where
  varTypeName _ = "NodeType!"

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
  show NodeTexts       = "NodeTexts"
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
  read "NodeTexts"         = Just NodeTexts
  read "Annuaire"          = Just Annuaire
  read "NodeFrameWrite"    = Just NodeFrameWrite
  read "NodeFrameCalc"     = Just NodeFrameCalc
  read "NodeFrameNotebook"     = Just NodeFrameNotebook
  read "NodeFrameVisio"    = Just NodeFrameVisio
  read "NodeFile"          = Just NodeFile
  -- TODO NodePublic read ?
  read _                   = Nothing

------------------------------------------------------

instance translateNodeType :: Translate NodeType where
  translate l n = case l of
    FR -> translateFR n
    _  -> translateEN n

translateFR :: NodeType -> String
translateFR = case _ of
  Annuaire            -> "Annuaire"
  Corpus              -> "Corpus"
  Dashboard           -> "Dashboard"
  Error               -> "Erreur"
  Folder              -> "Dossier"
  FolderPrivate       -> "Dossier privé"
  FolderPublic        -> "Dossier public"
  FolderShared        -> "Dossier partagé"
  Graph               -> "Graphe"
  Individu            -> "Individu"
  Node                -> "Nœud"
  NodeContact         -> "Contact"
  NodeList            -> "Liste"
  NodeUser            -> "Utilisateur"
  Nodes               -> "Nœuds"
  Phylo               -> "Phylo"
  Team                -> "Équipe"
  NodeTexts           -> "Docs"
  Tree                -> "Arbre"
  Url_Document        -> "Document URL"
  --
  NodeFile            -> "Fichier"
  NodeFrameCalc       -> "Feuilles de calcul"
  NodeFrameNotebook   -> "Carnet de notes"
  NodeFrameWrite      -> "Éditeur de texte"
  NodeFrameVisio      -> "Visio"
  NodePublic n        -> translateFR n

translateEN :: NodeType -> String
translateEN = case _ of
  Annuaire            -> "Annuaire"
  Corpus              -> "Corpus"
  Dashboard           -> "Dashboard"
  Error               -> "Error"
  Folder              -> "Folder"
  FolderPrivate       -> "Private folder"
  FolderPublic        -> "Public folder"
  FolderShared        -> "Shared folder"
  Graph               -> "Graph"
  Individu            -> "Person"
  Node                -> "Node"
  NodeContact         -> "Contact"
  NodeList            -> "List"
  NodeUser            -> "User"
  Nodes               -> "Nodes"
  Phylo               -> "Phylo"
  Team                -> "Team"
  NodeTexts           -> "Docs"
  Tree                -> "Tree"
  Url_Document        -> "URL document"
  --
  NodeFile            -> "File"
  NodeFrameCalc       -> "Calc"
  NodeFrameNotebook   -> "Notebook"
  NodeFrameWrite      -> "Write"
  NodeFrameVisio      -> "Visio"
  NodePublic n        -> translateEN n

------------------------------------------------------

getIcon :: NodeType -> Boolean -> String
getIcon NodeUser false = "user-circle"
getIcon NodeUser true  = "user"
------------------------------------------------------
getIcon Folder  false  = "folder"
getIcon Folder  true   = "folder-open-o"
------------------------------------------------------
getIcon FolderPrivate true  = "lock"
getIcon FolderPrivate false = "lock-circle"

getIcon FolderShared  true  = "share-alt"
getIcon FolderShared  false = "share-circle"
getIcon Team  true   = "users"
getIcon Team  false  = "users-closed"

getIcon FolderPublic true  = "globe-circle"
getIcon FolderPublic false = "globe"
------------------------------------------------------

getIcon Corpus true  = "book"
getIcon Corpus false = "book-circle"

getIcon Phylo _ = "code-fork"

getIcon Graph _ = "hubzilla"
getIcon NodeTexts _ = "newspaper-o"
getIcon Dashboard _ = "signal"
getIcon NodeList _ = "list"
getIcon NodeFile _ = "file"  -- TODO depending on mime type we can use fa-file-image etc

getIcon Annuaire true  = "address-card-o"
getIcon Annuaire false = "address-card"

getIcon NodeContact true  = "address-card-o"
getIcon NodeContact false = "address-card"

getIcon NodeFrameWrite true  = "file-text-o"
getIcon NodeFrameWrite false = "file-text"

getIcon NodeFrameCalc true  = "calculator"
getIcon NodeFrameCalc false = "calculator"

getIcon NodeFrameNotebook true  = "file-code-o"
getIcon NodeFrameNotebook false = "code"

getIcon NodeFrameVisio true  = "video-camera"
getIcon NodeFrameVisio false = "video-camera"



getIcon (NodePublic nt) b   = getIcon nt b

getIcon _        true   = "folder-open"
getIcon _        false  = "folder-o"

------------------------------------------------------

fldr :: NodeType -> Boolean -> String
fldr nt flag = classNamePrefix <> getIcon nt flag

charCodeIcon :: NodeType -> Boolean -> String
charCodeIcon nt flag = glyphiconToCharCode $ getIcon nt flag

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
nodeTypePath NodeTexts       = "texts"
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
instance Eq ScoreType where eq = genericEq
instance Show ScoreType where show = genericShow

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
instance Show OrderBy where show = genericShow
instance JSON.ReadForeign OrderBy where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign OrderBy where writeImpl = JSON.writeImpl <<< show

------------------------------------------------------------
-- V0 is the dummy case (impossible)
data ApiVersion = V0 | V10 | V11

derive instance Generic ApiVersion _
instance JSON.ReadForeign ApiVersion where
  readImpl f = do
    s <- JSON.readImpl f
    case s of
      "v0"   -> pure V0
      "v1.0" -> pure V10
      "v1.1" -> pure V11
      x      -> F.fail $ F.ErrorAtProperty x $ F.ForeignError "unknown API value"
instance JSON.WriteForeign ApiVersion where
  writeImpl v = F.unsafeToForeign $ JSON.writeImpl $ show v
instance Show ApiVersion where
  show V0  = "v0"
  show V10 = "v1.0"
  show V11 = "v1.1"
instance Eq ApiVersion where
  eq V10 V10 = true
  eq V11 V11 = true
  eq _ _ = false
------------------------------------------------------------

-- Types of ngrams. Used to display user-selectable tabs and is sent via API,
-- wrapped in `TabNgramType a :: TabSubType`
data CTabNgramType = CTabTerms | CTabSources | CTabAuthors | CTabInstitutes
derive instance Generic CTabNgramType _
derive instance Eq CTabNgramType
derive instance Ord CTabNgramType
instance Show CTabNgramType where
  show CTabTerms      = "Terms"
  show CTabSources    = "Sources"
  show CTabAuthors    = "Authors"
  show CTabInstitutes = "Institutes"
instance JSON.WriteForeign CTabNgramType where writeImpl = JSON.writeImpl <<< show

data PTabNgramType = PTabPatents | PTabBooks | PTabCommunication
derive instance Generic PTabNgramType _
instance Eq PTabNgramType where eq = genericEq
instance Ord PTabNgramType where compare = genericCompare
instance Show PTabNgramType where
  show PTabPatents       = "Patents"
  show PTabBooks         = "Books"
  show PTabCommunication = "Communication"
instance JSON.WriteForeign PTabNgramType where writeImpl = JSON.writeImpl <<< show

data TabSubType a = TabDocs | TabNgramType a | TabTrash | TabMoreLikeFav | TabMoreLikeTrash
derive instance Generic (TabSubType a) _
instance Eq a => Eq (TabSubType a) where eq = genericEq
instance Ord a => Ord (TabSubType a) where compare = genericCompare
instance JSON.WriteForeign a => JSON.WriteForeign (TabSubType a) where
  writeImpl TabDocs = JSON.writeImpl { type: "TabDocs"
                                     , data: (Nothing :: Maybe String) }
  writeImpl (TabNgramType a) = JSON.writeImpl { type: "TabNgramType"
                                              , data: a }
  writeImpl TabTrash = JSON.writeImpl { type: "TabTrash"
                                      , data: (Nothing :: Maybe String) }
  writeImpl TabMoreLikeFav = JSON.writeImpl { type: "TabMoreLikeFav"
                                            , data: (Nothing :: Maybe String) }
  writeImpl TabMoreLikeTrash = JSON.writeImpl { type: "TabMoreLikeTrash"
                                              , data: (Nothing :: Maybe String) }
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
instance Show TabType where show = genericShow
instance JSON.WriteForeign TabType where
  writeImpl (TabCorpus TabDocs)                         = JSON.writeImpl "Docs"
  writeImpl (TabCorpus (TabNgramType CTabAuthors))      = JSON.writeImpl "Authors"
  writeImpl (TabCorpus (TabNgramType CTabInstitutes))   = JSON.writeImpl "Institutes"
  writeImpl (TabCorpus (TabNgramType CTabSources))      = JSON.writeImpl "Sources"
  writeImpl (TabCorpus (TabNgramType CTabTerms))        = JSON.writeImpl "Terms"
  writeImpl (TabCorpus TabMoreLikeFav)                  = JSON.writeImpl "MoreFav"
  writeImpl (TabCorpus TabMoreLikeTrash)                = JSON.writeImpl "MoreTrash"
  writeImpl (TabCorpus TabTrash)                        = JSON.writeImpl "Trash"
  writeImpl (TabDocument TabDocs)                       = JSON.writeImpl "Docs"
  writeImpl (TabDocument (TabNgramType CTabAuthors))    = JSON.writeImpl "Authors"
  writeImpl (TabDocument (TabNgramType CTabInstitutes)) = JSON.writeImpl "Institutes"
  writeImpl (TabDocument (TabNgramType CTabSources))    = JSON.writeImpl "Sources"
  writeImpl (TabDocument (TabNgramType CTabTerms))      = JSON.writeImpl "Terms"
  writeImpl (TabDocument TabMoreLikeFav)                = JSON.writeImpl "MoreFav"
  writeImpl (TabDocument TabMoreLikeTrash)              = JSON.writeImpl "MoreTrash"
  writeImpl (TabDocument TabTrash)                      = JSON.writeImpl "Trash"
  writeImpl (TabPairing _d)                             = JSON.writeImpl "TabPairing"  -- TODO
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
type AffETableResult a = Aff (Either RESTError (TableResult a))

data Mode = Authors
          | Sources
          | Institutes
          | Terms

derive instance Generic Mode _
instance Show Mode where show = genericShow
instance Eq Mode where eq = genericEq
instance Ord Mode where compare = genericCompare
instance JSON.WriteForeign Mode where writeImpl = JSON.writeImpl <<< show

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
                   | CorpusFormUpload  -- this is file upload too
                   | GraphRecompute
                   | ListUpload
                   | ListCSVUpload  -- legacy v3 CSV upload for lists
                   | NodeDocument
                   | Query
                   | UpdateNgramsCharts
                   | UpdateNode
                   | UploadFile
                   | UploadFrameCalc

derive instance Generic AsyncTaskType _
instance JSON.ReadForeign AsyncTaskType where
  readImpl = JSONG.enumSumRep
instance Eq AsyncTaskType where
  eq = genericEq
instance Show AsyncTaskType where
  show = genericShow

asyncTaskTypePath :: AsyncTaskType -> String
asyncTaskTypePath AddNode            = "async/nobody/"
asyncTaskTypePath CorpusFormUpload   = "add/form/async/"
asyncTaskTypePath GraphRecompute     = "async/recompute/"
asyncTaskTypePath ListUpload         = "add/form/async/"
asyncTaskTypePath ListCSVUpload      = "csv/add/form/async/"
asyncTaskTypePath NodeDocument       = "document/upload/async"
asyncTaskTypePath Query              = "query/"
asyncTaskTypePath UpdateNgramsCharts = "ngrams/async/charts/update/"
asyncTaskTypePath UpdateNode         = "update/"
asyncTaskTypePath UploadFile         = "async/file/add/"
asyncTaskTypePath UploadFrameCalc    = "add/framecalc/async/"


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
instance Eq AsyncTask where eq = genericEq

newtype AsyncTaskWithType = AsyncTaskWithType
  { task :: AsyncTask
  , typ  :: AsyncTaskType
  }
derive instance Generic AsyncTaskWithType _
derive instance Newtype AsyncTaskWithType _
derive newtype instance JSON.ReadForeign AsyncTaskWithType
instance Eq AsyncTaskWithType where eq = genericEq

newtype AsyncProgress = AsyncProgress
  { id :: AsyncTaskID
  , log :: Array AsyncTaskLog
  , status :: AsyncTaskStatus
  }
derive instance Generic AsyncProgress _
derive instance Newtype AsyncProgress _
derive newtype instance JSON.ReadForeign AsyncProgress

newtype AsyncEvent = AsyncEvent
  { level :: String
  , message :: String
  }
derive instance Generic AsyncEvent _
derive instance Newtype AsyncEvent _
derive newtype instance JSON.ReadForeign AsyncEvent

newtype AsyncTaskLog = AsyncTaskLog
  { events :: Array AsyncEvent
  , failed :: Int
  , remaining :: Int
  , succeeded :: Int
  }
derive instance Generic AsyncTaskLog _
derive instance Newtype AsyncTaskLog _
derive newtype instance JSON.ReadForeign AsyncTaskLog

progressPercent :: AsyncProgress -> Number
progressPercent (AsyncProgress { log }) = perc
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
instance Eq SidePanelState where eq = genericEq

toggleSidePanelState :: SidePanelState -> SidePanelState
toggleSidePanelState InitialClosed = Opened
toggleSidePanelState Closed        = Opened
toggleSidePanelState Opened        = Closed

---------------------------------------------------------------------------

data FrontendError =
    FStringError { error :: String }
  | FRESTError { error :: RESTError }

derive instance Generic FrontendError _
instance Eq FrontendError where eq = genericEq
