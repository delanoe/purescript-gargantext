module Gargantext.Pages.Corpus.User.Users.Types.States  where

import Data.Maybe (Maybe(..))
import Gargantext.Pages.Corpus.User.Users.Types.Types (User)
import Gargantext.Pages.Folder as PS
import Gargantext.Pages.Corpus.User.Users.Specs.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = NoOp
  | ProjectsA PS.Action
  | TabA Tab.Action
  | FetchUser Int

type State =
  { activeTab :: Int
  , projects :: PS.State
  , user :: Maybe User
  }

initialState :: State
initialState =
  { activeTab : 0
  , projects : PS.initialState
  , user: Nothing
  }
