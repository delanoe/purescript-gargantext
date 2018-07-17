module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.Maybe (Maybe(Just))

import Gargantext.Components.Login              as LN
import Gargantext.Components.Modals.Modal          (modalShow)
import Gargantext.Components.Tree               as Tree
import Gargantext.Pages.Corpus                  as AC
import Gargantext.Pages.Corpus.Doc.Annotation   as D
import Gargantext.Pages.Corpus.Doc.Body         as CA
import Gargantext.Pages.Corpus.Doc.Document     as DV
import Gargantext.Pages.Corpus.Doc.Facets       as TV
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Corpus.User.Users       as U
import Gargantext.Pages.Home                    as L
import Gargantext.Pages.Search                  as S
import Gargantext.Router                           (Routes(..))

import Network.HTTP.Affjax (AJAX)
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
  , docAnnotationView :: D.State
  , ntreeView      :: Tree.State
  , tabview        :: TV.State
  , search         :: String
  , corpusAnalysis :: CA.State
  , showLogin      :: Boolean
  , showCorpus     :: Boolean
  , graphExplorer  :: GE.State
  , initialized    :: Boolean
  , ngState        :: NG.State
  , dashboard      :: Dsh.State
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
  , docAnnotationView : D.initialState
  , ntreeView      : Tree.exampleTree
  , tabview        : TV.initialState
  , search         : ""
  , corpusAnalysis : CA.initialState
  , showLogin      : false
  , showCorpus     : false
  , graphExplorer  : GE.initialState
  , initialized    : false
  , ngState        : NG.initialState
  , dashboard      : Dsh.initialState
  }

---------------------------------------------------------
_landingState :: Lens' AppState L.State
_landingState = lens (\s -> s.landingState) (\s ss -> s{landingState = ss})

_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_addCorpusState :: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})

_docViewState :: Lens' AppState DV.State
_docViewState = lens (\s -> s.docViewState) (\s ss -> s{docViewState = ss})

_searchState :: Lens' AppState S.State
_searchState = lens (\s -> s.searchState) (\s ss -> s{searchState = ss})

_userPageState :: Lens' AppState U.State
_userPageState = lens (\s -> s.userPage) (\s ss -> s{userPage = ss})

_docAnnotationViewState :: Lens' AppState D.State
_docAnnotationViewState = lens (\s -> s.docAnnotationView) (\s ss -> s{docAnnotationView = ss})

_treeState :: Lens' AppState Tree.State
_treeState = lens (\s -> s.ntreeView) (\s ss -> s {ntreeView = ss})

_tabviewState :: Lens' AppState TV.State
_tabviewState = lens (\s -> s.tabview) (\s ss -> s {tabview = ss})

_corpusState :: Lens' AppState CA.State
_corpusState = lens (\s -> s.corpusAnalysis) (\s ss -> s {corpusAnalysis = ss})

_dashBoardSate :: Lens' AppState Dsh.State
_dashBoardSate = lens (\s -> s.dashboard) (\s ss -> s {dashboard = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorer) (\s ss -> s{graphExplorer = ss})

_ngState :: Lens' AppState NG.State
_ngState = lens (\s -> s.ngState) (\s ss -> s{ngState = ss})


