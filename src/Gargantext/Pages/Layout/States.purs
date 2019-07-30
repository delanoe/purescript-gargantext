module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Data.Lens                                       (Lens', lens)
import Data.Maybe                                      (Maybe(Just))
import Effect (Effect)
import Gargantext.Components.Login                  as LN
import Gargantext.Config (EndConfig, endConfigStateful)

import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Router                               (Routes(..))

type AppState =
  { currentRoute       :: Maybe Routes
  , loginState         :: LN.State
  , showLogin          :: Boolean
  , showCorpus         :: Boolean
  , graphExplorerState :: GE.State
  , showTree           :: Boolean
  , endConfig          :: EndConfig
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
    , endConfig : endConfigStateful
    }


---------------------------------------------------------

_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorerState) (\s ss -> s{graphExplorerState = ss})

