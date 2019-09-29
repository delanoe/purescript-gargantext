module Gargantext.Routes where

import Prelude
import Data.Maybe (Maybe)
import Gargantext.Types

data AppRoute
  = Home
  | Login
  | Folder Int
  | Corpus Int
    | Document       Int Int
    | CorpusDocument Int Int Int
    | PGraphExplorer Int
    | Dashboard
    | Texts          Int
    | Lists          Int
  | Annuaire Int
    | UserPage       Int
    | ContactPage    Int

data SessionRoute
  = Tab TabType (Maybe Id)
  | Children NodeType Offset Limit (Maybe OrderBy) (Maybe Id)
  | GetNgrams NgramsGetOpts (Maybe Id)
  | PutNgrams TabType (Maybe ListId) (Maybe TermList) (Maybe Id)
  -- ^ This name is not good. In particular this URL is used both in PUT and POST.
  | NodeAPI NodeType (Maybe Id)
  | ListDocument (Maybe ListId) (Maybe Id)
  | Search SearchOpts (Maybe Id)
  | CorpusMetrics CorpusMetricOpts  (Maybe Id)
  | Chart ChartOpts (Maybe Id)

instance showAppRoute :: Show AppRoute where
  show Home             = "Home"
  show Login            = "Login"
  show (Folder   i)     = "Folder"   <> show i
  show (Corpus i)       = "Corpus"   <> show i
  show (Document _ i)     = "Document" <> show i
  show (CorpusDocument _ _ i) = "Document" <> show i
  show (PGraphExplorer i) = "graphExplorer" <> show i
  show Dashboard        = "Dashboard"
  show (Texts i)          = "texts" <> show i
  show (Lists i)          = "lists" <> show i
  show (Annuaire i)     = "Annuaire" <> show i
  show (UserPage i)     = "User"     <> show i
  show (ContactPage i)  = "Contact"  <> show i

appPath :: AppRoute -> String
appPath Home = ""
appPath Login = "login"
appPath (Folder i) = "folder/" <> show i
appPath (CorpusDocument c l i) = "corpus/" <> show c <> "/list/" <> show l <> "/document/" <> show i
appPath (Corpus i) = "corpus/" <> show i
appPath (Document l i) = "list/" <> show l <> "/document/" <> show i
appPath Dashboard = "dashboard"
appPath (PGraphExplorer i) = "graph/" <> show i
appPath (Texts i) = "texts/" <> show i
appPath (Lists i) = "lists/" <> show i
appPath (Annuaire i) = "annuaire/" <> show i
appPath (UserPage i) = "user/" <> show i
appPath (ContactPage i) = "contact/" <> show i
