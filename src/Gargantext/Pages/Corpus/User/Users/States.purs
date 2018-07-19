module Gargantext.Pages.Corpus.User.Users.States  where

import Gargantext.Pages.Corpus.User.Brevets as B
import Data.Maybe (Maybe(..))
import Gargantext.Pages.Corpus.User.Users.Types.Types (User)
import Gargantext.Pages.Folder as PS
import Gargantext.Pages.Corpus.Doc.Facets.Documents as P
import Gargantext.Components.Tab as Tab

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

------------------------------------------------------

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_publens :: Lens' State P.State
_publens = lens (\s -> s.publications) (\s ss -> s { publications= ss})

_brevetslens :: Lens' State B.State
_brevetslens = lens (\s -> s.brevets) (\s ss -> s {brevets = ss})

_projectslens :: Lens' State PS.State
_projectslens = lens (\s -> s.projects) (\s ss -> s {projects = ss})

_user :: Lens' State (Maybe User)
_user = lens (\s -> s.user) (\s ss -> s{user = ss})
