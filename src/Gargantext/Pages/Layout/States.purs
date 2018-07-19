module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(Just))

import Gargantext.Components.Login              as LN
import Gargantext.Components.Tree               as Tree
import Gargantext.Pages.Layout.Specs.AddCorpus  as AC
import Gargantext.Pages.Corpus                  as CA
import Gargantext.Pages.Corpus.Doc.Annotation   as D
import Gargantext.Pages.Corpus.Doc.Facets       as TV
import Gargantext.Pages.Corpus.Doc.Facets.Documents  as DV
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Corpus.User.Users       as U
import Gargantext.Pages.Home                    as L
import Gargantext.Pages.Layout.Specs.Search     as S
import Gargantext.Router                           (Routes(..))

import Network.HTTP.Affjax (AJAX)

type E e = (dom :: DOM, ajax :: AJAX, console :: CONSOLE | e)

type AppState =
  { currentRoute   :: Maybe Routes
  , landingState   :: L.State
  ,   loginState   :: LN.State
  , addCorpusState :: AC.State
  , docViewState   :: DV.State
  , searchState    :: S.State
  , userPageState  :: U.State
  , docAnnotationState :: D.State
  , ntreeState     :: Tree.State
  , tabviewState   :: TV.State
  , search         :: String
  , corpusState    :: CA.State
  , showLogin      :: Boolean
  , showCorpus     :: Boolean
  , graphExplorerState  :: GE.State
  , initialized    :: Boolean
  , ngramState     :: NG.State
  , dashboardState :: Dsh.State
  }

initAppState :: AppState
initAppState =
  { currentRoute   : Just Home
  , landingState   : L.initialState
  , loginState     : LN.initialState
  , addCorpusState : AC.initialState
  , docViewState   : DV.tdata
  , searchState    : S.initialState
  , userPageState  : U.initialState
  , docAnnotationState : D.initialState
  , ntreeState   : Tree.exampleTree
  , tabviewState : TV.initialState
  , search  : ""
  , corpusState  : CA.initialState
  , showLogin    : false
  , showCorpus   : false
  , graphExplorerState  : GE.initialState
  , initialized    : false
  , ngramState     : NG.initialState
  , dashboardState : Dsh.initialState
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
_userPageState = lens (\s -> s.userPageState) (\s ss -> s{userPageState = ss})

_docAnnotationViewState :: Lens' AppState D.State
_docAnnotationViewState = lens (\s -> s.docAnnotationState) (\s ss -> s{docAnnotationState = ss})

_treeState :: Lens' AppState Tree.State
_treeState = lens (\s -> s.ntreeState) (\s ss -> s {ntreeState = ss})

_tabviewState :: Lens' AppState TV.State
_tabviewState = lens (\s -> s.tabviewState) (\s ss -> s {tabviewState = ss})

_corpusState :: Lens' AppState CA.State
_corpusState = lens (\s -> s.corpusState) (\s ss -> s {corpusState = ss})

_dashBoardSate :: Lens' AppState Dsh.State
_dashBoardSate = lens (\s -> s.dashboardState) (\s ss -> s {dashboardState = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorerState) (\s ss -> s{graphExplorerState = ss})

_ngramState :: Lens' AppState NG.State
_ngramState = lens (\s -> s.ngramState) (\s ss -> s{ngramState = ss})


