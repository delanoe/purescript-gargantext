module Gargantext.Pages.Layout.States where

import Prelude hiding (div)

import Data.Lens                                       (Lens', lens)
import Data.Maybe                                      (Maybe(..))
import Effect (Effect)

import Gargantext.Components.Login                  as LN
--import Gargantext.Components.Login.Types            as LNT
--import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Router                               (Routes(..))

type AppState =
  { currentRoute       :: Maybe Routes
  , loginState         :: LN.State
  , showLogin          :: Boolean
  , showCorpus         :: Boolean
  --, graphExplorerState :: Record GET.StateGlue
  , showTree           :: Boolean
  }

initAppState :: Effect AppState
initAppState = do
  loginState <- LN.initialState

  pure
    { currentRoute   : Just Home
    , loginState
    , showLogin      : false
    , showCorpus     : false
    --, graphExplorerState : GET.initialStateGlue
    , showTree : false
    }


---------------------------------------------------------

_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_graphExplorerState :: Lens' AppState (Record GET.StateGlue)
_graphExplorerState = lens getter setter
  where
    getter :: AppState -> Record GET.StateGlue
    getter s = {
    }
    --setter s ss = s {graphExplorerState = ss}
    setter :: AppState -> (Record GET.StateGlue) -> AppState
    setter s ss = s

