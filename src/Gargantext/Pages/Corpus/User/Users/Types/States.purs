module Gargantext.Pages.Corpus.User.Users.Types.States  where

import Gargantext.Pages.Corpus.User.Brevets as B
import Data.Maybe (Maybe(..))
import Gargantext.Pages.Corpus.User.Users.Types.Types (User)
import Gargantext.Pages.Folder as PS
import Gargantext.Pages.Corpus.User.Users.Specs.Documents as P
import Gargantext.Components.Tab as Tab

data Action
  = NoOp
  | PublicationA P.Action
  | BrevetsA B.Action
  | ProjectsA PS.Action
  | TabA Tab.Action
  | FetchUser Int

type State =
  { activeTab :: Int
  , publications :: P.State
  , brevets :: B.State
  , projects :: PS.State
  , user :: Maybe User
  }

initialState :: State
initialState =
  { activeTab : 0
  , publications : P.initialState
  , brevets : B.initialState
  , projects : PS.initialState
  , user: Nothing
  }
