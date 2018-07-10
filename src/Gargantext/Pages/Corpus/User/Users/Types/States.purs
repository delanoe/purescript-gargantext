module Gargantext.Users.Types.States
       where

import Brevets as B
import Data.Maybe (Maybe(..))
import Gargantext.Users.Types.Types (User)
import Projects as PS
import Publications as P
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
