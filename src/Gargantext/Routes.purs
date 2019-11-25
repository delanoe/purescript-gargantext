module Gargantext.Routes where

import Prelude
import Data.Maybe (Maybe)
import Gargantext.Types (ChartOpts, CorpusMetricOpts, Id, Limit, ListId, NgramsGetOpts, NodeType, Offset, OrderBy, SearchOpts, SessionId, TabType, TermList)

data AppRoute
  = Home
  | Login
  | Folder SessionId Int
  | Corpus SessionId  Int
    | Document       SessionId Int Int
    | CorpusDocument SessionId Int Int Int
    | PGraphExplorer SessionId Int
    | Dashboard      SessionId Int
    | Texts          SessionId Int
    | Lists          SessionId Int
  | Annuaire SessionId Int
    | UserPage       SessionId Int
    | ContactPage    SessionId AnnuaireId ContactId

type AnnuaireId = Int
type ContactId = Int

data SessionRoute
  = Tab TabType (Maybe Id)
  | Children NodeType Offset Limit (Maybe OrderBy) (Maybe Id)
  | GetNgrams NgramsGetOpts (Maybe Id)
  | PutNgrams TabType (Maybe ListId) (Maybe TermList) (Maybe Id)
  -- ^ This name is not good. In particular this URL is used both in PUT and POST.
  | NodeAPI NodeType (Maybe Id) String
  | ListDocument (Maybe ListId) (Maybe Id)
  | Search SearchOpts (Maybe Id)
  | CorpusMetrics CorpusMetricOpts  (Maybe Id)
  | Chart ChartOpts (Maybe Id)

instance showAppRoute :: Show AppRoute where
  show Home                     = "Home"
  show Login                    = "Login"
  show (Folder s i)             = "Folder"   <> show i <> " (" <> show s <> ")"
  show (Corpus s i)             = "Corpus"   <> show i <> " (" <> show s <> ")"
  show (Document _ s i)         = "Document" <> show i <> " (" <> show s <> ")"
  show (CorpusDocument s _ _ i) = "CorpusDocument" <> show i <> " (" <> show s <> ")"
  show (PGraphExplorer s i)     = "graphExplorer"  <> show i <> " (" <> show s <> ")"
  show (Dashboard      s i)     = "Dashboard"      <> show i <> " (" <> show s <> ")"
  show (Texts s i)              = "texts"    <> show i <> " (" <> show s <> ")"
  show (Lists s i)              = "lists"    <> show i <> " (" <> show s <> ")"
  show (Annuaire s i)           = "Annuaire" <> show i <> " (" <> show s <> ")"
  show (UserPage s i)           = "User"     <> show i <> " (" <> show s <> ")"
  show (ContactPage s _a i)      = "Contact"  <> show i <> " (" <> show s <> ")"

appPath :: AppRoute -> String
appPath Home                 = ""
appPath Login                = "login"
appPath (Folder s i)         = "folder/"     <> show s <> "/" <> show i
appPath (CorpusDocument s c l i) = "corpus/" <> show s <> "/" <> show c <> "/list/" <> show l <> "/document/" <> show i
appPath (Corpus s i)         = "corpus/"     <> show s <> "/" <> show i
appPath (Document s l i)     = "list/"       <> show s <> "/" <> show l <> "/document/" <> show i
appPath (Dashboard s i)      = "dashboard/"  <> show s <> "/" <> show i
appPath (PGraphExplorer s i) = "graph/"      <> show s <> "/" <> show i
appPath (Texts s i)          = "texts/"      <> show s <> "/" <> show i
appPath (Lists s i)          = "lists/"      <> show s <> "/" <> show i
appPath (Annuaire s i)       = "annuaire/"   <> show s <> "/" <> show i
appPath (UserPage s i)       = "user/"       <> show s <> "/" <> show i
appPath (ContactPage s a i)  = "annuaire/"   <> show s <> "/" <> show a <> "/contact/" <> show i
