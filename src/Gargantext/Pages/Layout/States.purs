module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Data.Lens                                       (Lens', lens)
import Data.Maybe                                      (Maybe(Just))
import Effect (Effect)
import Gargantext.Components.Login                  as LN

import Gargantext.Pages.Corpus.Document       as D
import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Pages.Annuaire.User.Contacts      as C
import Gargantext.Pages.Layout.Specs.AddCorpus      as AC
import Gargantext.Pages.Layout.Specs.Search         as S
import Gargantext.Router                               (Routes(..))

type AppState =
  { currentRoute   :: Maybe Routes
  , loginState   :: LN.State
  , addCorpusState :: AC.State
  , searchState    :: S.State
  , userPageState  :: C.State
  , documentState  :: D.State
  , search         :: String
  , showLogin      :: Boolean
  , showCorpus     :: Boolean
  , graphExplorerState  :: GE.State
  , showTree :: Boolean
  }

initAppState :: Effect AppState
initAppState = do
  loginState <- LN.initialState
  pure
    { currentRoute   : Just Home
    , loginState
    , addCorpusState : AC.initialState
    , searchState    : S.initialState
    , userPageState  : C.initialState
    , documentState  : D.initialState {}
    , search         : ""
    , showLogin      : false
    , showCorpus     : false
    , graphExplorerState : GE.initialState
    , showTree : false
    }

---------------------------------------------------------
_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_addCorpusState :: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})

_searchState :: Lens' AppState S.State
_searchState = lens (\s -> s.searchState) (\s ss -> s{searchState = ss})

_userPageState :: Lens' AppState C.State
_userPageState = lens (\s -> s.userPageState) (\s ss -> s{userPageState = ss})

_documentViewState :: Lens' AppState D.State
_documentViewState = lens (\s -> s.documentState) (\s ss -> s{documentState = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorerState) (\s ss -> s{graphExplorerState = ss})
