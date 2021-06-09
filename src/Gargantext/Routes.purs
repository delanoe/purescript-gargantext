module Gargantext.Routes where

import Prelude

import Data.Maybe (Maybe(..))

import Gargantext.Types (ChartOpts, ChartType, CorpusMetricOpts, CTabNgramType, Id, Limit,
                         ListId, DocId, ContactId, NgramsGetOpts, NgramsGetTableAllOpts, NodeType,
                         Offset, OrderBy, SearchOpts, SessionId, TabSubType, TabType, TermList)
import Gargantext.Types as GT

data AppRoute
  = Annuaire       SessionId Int
  | ContactPage    SessionId Int Int
  | Corpus         SessionId Int
  | CorpusDocument SessionId Int Int Int
  | Dashboard      SessionId Int
  | Document       SessionId Int Int
  | Folder         SessionId Int
  | FolderPrivate  SessionId Int
  | FolderPublic   SessionId Int
  | FolderShared   SessionId Int
  | Home
  | Lists          SessionId Int
  | Login
  | PGraphExplorer  SessionId Int
  | PhyloExplorer   SessionId Int
  | RouteFile       SessionId Int
  | RouteFrameCalc  SessionId Int
  | RouteFrameCode  SessionId Int
  | RouteFrameWrite SessionId Int
  | Team            SessionId Int
  | Texts           SessionId Int
  | UserPage        SessionId Int


derive instance eqAppRoute :: Eq AppRoute

data SessionRoute
  = Tab TabType (Maybe Id)
  | Children NodeType Offset Limit (Maybe OrderBy) (Maybe Id)
  | GetNgrams NgramsGetOpts (Maybe Id)
  | GetNgramsTableAll NgramsGetTableAllOpts (Maybe Id)
  | GetNgramsTableVersion { listId :: ListId, tabType :: TabType } (Maybe Id)
  | PutNgrams TabType (Maybe ListId) (Maybe TermList) (Maybe Id)
  | PostNgramsChartsAsync (Maybe Id)
  -- ^ This name is not good. In particular this URL is used both in PUT and POST.
  | RecomputeNgrams   (TabSubType CTabNgramType) Id ListId
  | RecomputeListChart ChartType  CTabNgramType  Id ListId
  | NodeAPI       NodeType (Maybe Id) String
  | TreeFirstLevel (Maybe Id) String
  | GraphAPI      Id String
  | ListsRoute    ListId
  | ListDocument (Maybe ListId) (Maybe DocId)
  | Search        SearchOpts (Maybe Id)
  | CorpusMetrics CorpusMetricOpts  (Maybe Id)
  | CorpusMetricsHash { listId :: ListId, tabType :: TabType }  (Maybe Id)
  | Chart ChartOpts (Maybe Id)
  | ChartHash { chartType :: ChartType, listId :: Maybe ListId, tabType :: TabType } (Maybe Id)
  -- | AnnuaireContact AnnuaireId DocId

instance showAppRoute :: Show AppRoute where
  show Home                     = "Home"
  show Login                    = "Login"
  show (Folder        s i)      = "Folder"         <> show i <> " (" <> show s <> ")"
  show (FolderPrivate s i)      = "FolderPrivate"  <> show i <> " (" <> show s <> ")"
  show (FolderPublic  s i)      = "FolderPublic"   <> show i <> " (" <> show s <> ")"
  show (FolderShared  s i)      = "FolderShared"   <> show i <> " (" <> show s <> ")"
  show (Team          s i)      = "Team"           <> show i <> " (" <> show s <> ")"
  show (Corpus        s i)      = "Corpus"         <> show i <> " (" <> show s <> ")"
  show (Document    _ s i)      = "Document"       <> show i <> " (" <> show s <> ")"
  show (CorpusDocument s _ _ i) = "CorpusDocument" <> show i <> " (" <> show s <> ")"
  show (PGraphExplorer s i)     = "graphExplorer"  <> show i <> " (" <> show s <> ")"
  show (PhyloExplorer  s i)     = "phyloExplorer"  <> show i <> " (" <> show s <> ")"
  show (Dashboard      s i)     = "Dashboard"      <> show i <> " (" <> show s <> ")"
  show (Texts          s i)     = "texts"          <> show i <> " (" <> show s <> ")"
  show (Lists          s i)     = "lists"          <> show i <> " (" <> show s <> ")"
  show (Annuaire       s i)     = "Annuaire"       <> show i <> " (" <> show s <> ")"
  show (UserPage       s i)     = "User"           <> show i <> " (" <> show s <> ")"
  show (ContactPage  s a i)     = "Contact"        <> show a <> "::" <> show i <> " (" <> show s <> ")"
  show (RouteFrameWrite s i)    = "write"          <> show i <> " (" <> show s <> ")"
  show (RouteFrameCalc  s i)    = "calc"           <> show i <> " (" <> show s <> ")"
  show (RouteFrameCode  s i)    = "code"           <> show i <> " (" <> show s <> ")"
  show (RouteFile       s i)    = "file"           <> show i <> " (" <> show s <> ")"


appPath :: AppRoute -> String
appPath Home                     = ""
appPath Login                    = "login"
appPath (Folder s i)             = "folder/"        <> show s <> "/" <> show i
appPath (FolderPrivate s i)      = "folderPrivate/" <> show s <> "/" <> show i
appPath (FolderPublic s i)       = "folderPublic/"  <> show s <> "/" <> show i
appPath (FolderShared s i)       = "folderShared/"  <> show s <> "/" <> show i
appPath (Team s i)               = "team/"          <> show s <> "/" <> show i
appPath (CorpusDocument s c l i) = "corpus/" <> show s <> "/" <> show c <> "/list/" <> show l <> "/document/" <> show i
appPath (Corpus s i)             = "corpus/"     <> show s <> "/" <> show i
appPath (Document s l i)         = "list/"       <> show s <> "/" <> show l <> "/document/" <> show i
appPath (Dashboard s i)          = "dashboard/"  <> show s <> "/" <> show i
appPath (PGraphExplorer s i)     = "graph/"      <> show s <> "/" <> show i
appPath (PhyloExplorer  s i)     = "phylo/"      <> show s <> "/" <> show i
appPath (Texts s i)              = "texts/"      <> show s <> "/" <> show i
appPath (Lists s i)              = "lists/"      <> show s <> "/" <> show i
appPath (Annuaire s i)           = "annuaire/"   <> show s <> "/" <> show i
appPath (UserPage s i)           = "user/"       <> show s <> "/" <> show i
appPath (ContactPage s a i)      = "annuaire/"   <> show s <> "/" <> show a <> "/contact/" <> show i
appPath (RouteFrameWrite s i)    = "write/"     <> show s <> "/" <> show i
appPath (RouteFrameCalc s i)     = "calc/"      <> show s <> "/" <> show i
appPath (RouteFrameCode s i)     = "code/"      <> show s <> "/" <> show i
appPath (RouteFile s i)          = "file/"      <> show s <> "/" <> show i

nodeTypeAppRoute :: NodeType -> SessionId -> Int -> Maybe AppRoute
nodeTypeAppRoute GT.Annuaire s i       = Just $ Annuaire s i
nodeTypeAppRoute GT.Corpus s i         = Just $ Corpus s i
nodeTypeAppRoute GT.Dashboard s i      = Just $ Dashboard s i
nodeTypeAppRoute GT.Folder s i         = Just $ Folder s i
nodeTypeAppRoute GT.FolderPrivate s i  = Just $ FolderPrivate s i
nodeTypeAppRoute GT.FolderPublic s i   = Just $ FolderPublic s i
nodeTypeAppRoute GT.FolderShared s i   = Just $ FolderShared s i
nodeTypeAppRoute GT.Graph s i          = Just $ PGraphExplorer s i
nodeTypeAppRoute GT.Phylo s i          = Just $ PhyloExplorer  s i
nodeTypeAppRoute GT.NodeContact s i    = Just $ Annuaire s i
nodeTypeAppRoute GT.NodeFile s i       = Just $ RouteFile s i
nodeTypeAppRoute GT.NodeList s i       = Just $ Lists s i
nodeTypeAppRoute GT.NodeUser s i       = Just $ UserPage s i
nodeTypeAppRoute GT.Team s i           = Just $ Team s i
nodeTypeAppRoute GT.Texts s i          = Just $ Texts s i
nodeTypeAppRoute GT.NodeFrameWrite s i = Just $ RouteFrameWrite s i
nodeTypeAppRoute GT.NodeFrameCalc s i  = Just $ RouteFrameCalc s i
nodeTypeAppRoute _ _ _                 = Nothing
