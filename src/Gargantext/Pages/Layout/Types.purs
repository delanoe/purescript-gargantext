module Gargantext.Layout.Types where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Gargantext.Pages.Corpus.Doc.Body as CA
import DOM (DOM)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe(Just))
import Gargantext.Pages.Corpus.Doc.Annotation as D
import Gargantext.Pages.Corpus.Doc.Document as DV
import Gargantext.Components.Login as LN
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Tree as Tree
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus as AC
import Gargantext.Router (Routes(..))
import Gargantext.Pages.Corpus.User.Users as U
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Home as L
import Network.HTTP.Affjax (AJAX)
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Search as S
import Gargantext.Pages.Corpus.Doc.Facets as TV
import Thermite (PerformAction, modifyState)

type E e = (dom :: DOM, ajax :: AJAX, console :: CONSOLE | e)

type AppState =
  { currentRoute   :: Maybe Routes
  , landingState   :: L.State
  , loginState     :: LN.State
  , addCorpusState :: AC.State
  , docViewState   :: DV.State
  , searchState    :: S.State
  , userPage       :: U.State
  , docAnnotationView   :: D.State
  , ntreeView   :: Tree.State
  , tabview :: TV.State
  , search :: String
  , corpusAnalysis :: CA.State
  , showLogin :: Boolean
  , showCorpus :: Boolean
  , graphExplorer :: GE.State
  , initialized :: Boolean
  , ngState :: NG.State
  , dashboard :: Dsh.State
  }

initAppState :: AppState
initAppState =
  { currentRoute   : Just Home
  , landingState   : L.initialState
  , loginState     : LN.initialState
  , addCorpusState : AC.initialState
  , docViewState   : DV.tdata
  , searchState    : S.initialState
  , userPage       : U.initialState
  , docAnnotationView   : D.initialState
  , ntreeView : Tree.exampleTree
  , tabview : TV.initialState
  , search : ""
  , corpusAnalysis : CA.initialState
  , showLogin : false
  , showCorpus : false
  , graphExplorer : GE.initialState
  , initialized : false
  , ngState : NG.initialState
  , dashboard : Dsh.initialState
  }

data Action
  = Initialize
  | LandingA   L.Action
  | LoginA     LN.Action
  | SetRoute   Routes
  | AddCorpusA AC.Action
  | DocViewA   DV.Action
  | SearchA    S.Action
  | UserPageA  U.Action
  | DocAnnotationViewA  D.Action
  | TreeViewA  Tree.Action
  | TabViewA TV.Action
  | GraphExplorerA GE.Action
  | DashboardA Dsh.Action
  | Search String
  | Go
  | CorpusAnalysisA CA.Action
  | ShowLogin
  | ShowAddcorpus
  | NgramsA  NG.Action
