module Gargantext.Pages.Corpus.User.Users.Types.States  where

import Data.Maybe (Maybe(..))
import Gargantext.Pages.Corpus.User.Users.Types.Types (User)
import Gargantext.Pages.Folder as PS
import Gargantext.Pages.Corpus.User.Users.Specs.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = NoOp
  | PublicationA P.Action
  | ProjectsA PS.Action
  | TabA Tab.Action
  | FetchUser Int

type State =
  { activeTab :: Int
  , publications :: P.State
  , projects :: PS.State
  , user :: Maybe User
  }

initialState :: State
initialState =
  { activeTab : 0
  , publications : P.initialState
  , projects : PS.initialState
  , user: Nothing
  }
