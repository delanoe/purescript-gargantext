module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Data.Lens                                       (Lens', lens)
import Data.Maybe                                      (Maybe(Just))
import Gargantext.Components.Login                  as LN
import Gargantext.Components.Tree                   as Tree

import Gargantext.Pages.Corpus                      as Corpus
import Gargantext.Pages.Corpus.Document       as D
import Gargantext.Pages.Annuaire                    as Annuaire
import Gargantext.Pages.Corpus.Tabs.Documents as DV
import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Pages.Annuaire.User.Users         as U
import Gargantext.Pages.Layout.Specs.AddCorpus      as AC
import Gargantext.Pages.Layout.Specs.Search         as S
import Gargantext.Router                               (Routes(..))

type AppState =
  { currentRoute   :: Maybe Routes
  , loginState   :: LN.State
  , corpus         :: Corpus.State
  , addCorpusState :: AC.State
  , docViewState   :: DV.State
  , searchState    :: S.State
  , userPageState  :: U.State
  , documentState :: D.State
  , annuaireState  :: Annuaire.State
  , ntreeState     :: Tree.State
  , search         :: String
  , showLogin      :: Boolean
  , showCorpus     :: Boolean
  , graphExplorerState  :: GE.State
  , initialized    :: Boolean
  }

initAppState :: AppState
initAppState =
  { currentRoute   : Just Home
  , corpus         : Corpus.initialState
  , loginState     : LN.initialState
  , addCorpusState : AC.initialState
  , docViewState   : DV.initialState
  , searchState    : S.initialState
  , userPageState  : U.initialState
  , documentState : D.initialState
  , ntreeState     : Tree.exampleTree
  , annuaireState  : Annuaire.initialState
  , search         : ""
  , showLogin      : false
  , showCorpus     : false
  , graphExplorerState : GE.initialState
  , initialized    : false
  }

---------------------------------------------------------
_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_addCorpusState :: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})

_corpusState :: Lens' AppState Corpus.State
_corpusState = lens (\s -> s.corpus) (\s ss -> s{corpus = ss})

_docViewState :: Lens' AppState DV.State
_docViewState = lens (\s -> s.docViewState) (\s ss -> s{docViewState = ss})

_searchState :: Lens' AppState S.State
_searchState = lens (\s -> s.searchState) (\s ss -> s{searchState = ss})

_userPageState :: Lens' AppState U.State
_userPageState = lens (\s -> s.userPageState) (\s ss -> s{userPageState = ss})

_annuaireState :: Lens' AppState Annuaire.State
_annuaireState = lens (\s -> s.annuaireState) (\s ss -> s{annuaireState = ss})

_documentViewState :: Lens' AppState D.State
_documentViewState = lens (\s -> s.documentState) (\s ss -> s{documentState = ss})

_treeState :: Lens' AppState Tree.State
_treeState = lens (\s -> s.ntreeState) (\s ss -> s {ntreeState = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorerState) (\s ss -> s{graphExplorerState = ss})
