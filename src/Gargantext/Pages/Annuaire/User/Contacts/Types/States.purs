module Gargantext.Pages.Annuaire.User.Contacts.Types.States  where

import Data.Maybe (Maybe(..))
import Gargantext.Pages.Annuaire.User.Contacts.Types.Types (Contact)
import Gargantext.Pages.Annuaire.User.Contacts.Specs.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = TabA Tab.Action
  | FetchContact Int

type State =
  { activeTab :: Int
  , contact :: Maybe Contact
  }

initialState :: State
initialState =
  { activeTab : 0
  , contact : Nothing
  }
