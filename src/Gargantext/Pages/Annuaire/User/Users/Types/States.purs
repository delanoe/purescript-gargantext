module Gargantext.Pages.Annuaire.User.Users.Types.States  where

import Data.Maybe (Maybe(..))
import Gargantext.Pages.Annuaire.User.Users.Types.Types (User)
import Gargantext.Pages.Annuaire.User.Users.Specs.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = TabA Tab.Action
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
