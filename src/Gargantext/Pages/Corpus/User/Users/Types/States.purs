module Gargantext.Pages.Corpus.User.Users.Types.States  where

import Data.Maybe (Maybe(..))
import Gargantext.Pages.Corpus.User.Users.Types.Types (User)
import Gargantext.Pages.Corpus.User.Users.Specs.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = NoOp
  | TabA Tab.Action
  | FetchUser Int

type State =
  { activeTab :: Int
  , user :: Maybe User
  }

initialState :: State
initialState =
  { activeTab : 0
  , user: Nothing
  }
