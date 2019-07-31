module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Data.Lens                                       (Lens', lens)
import Data.Maybe                                      (Maybe(Just))
import Effect (Effect)
import Gargantext.Config as C
import Gargantext.Components.Login                  as LN

import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Router                               (Routes(..))

type AppState =
  { currentRoute       :: Maybe Routes
  , loginState         :: LN.State
  , showLogin          :: Boolean
  , showCorpus         :: Boolean
  , graphExplorerState :: GE.State
  , showTree           :: Boolean
  , configState     :: C.State
  }

initAppState :: Effect AppState
initAppState = do
  loginState <- LN.initialState
  pure
    { currentRoute   : Just Home
    , loginState
    , showLogin      : false
    , showCorpus     : false
    , graphExplorerState : GE.initialState
    , showTree : false
    , configState : C.initialState
    }


---------------------------------------------------------

_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_configState :: Lens' AppState C.State
_configState = lens (\s -> s.configState) (\s ss -> s{configState = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorerState) (\s ss -> s{graphExplorerState = ss})

