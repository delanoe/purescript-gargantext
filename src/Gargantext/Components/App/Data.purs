module Gargantext.Components.App.Data (App, Cursors, emptyApp) where

import Data.Set as Set
import Data.Maybe (Maybe(..))
import Toestand as T

import Gargantext.Sessions as Sessions
import Gargantext.Sessions (OpenNodes, Sessions)
import Gargantext.Routes (AppRoute(Home))
import Gargantext.Types (Handed(RightHanded))
import Gargantext.Utils.Toestand as T2

type App =
  { handed       :: Handed
  , forestOpen   :: OpenNodes
  , reloadRoot   :: Int
  , reloadForest :: Int
  , route        :: AppRoute
  , sessions     :: Sessions
  , showCorpus   :: Boolean
  , showLogin    :: Boolean
  }

emptyApp :: App
emptyApp =
  { handed:       RightHanded
  , route:        Home
  , forestOpen:   Set.empty
  , reloadRoot:   T2.newReload
  , reloadForest: T2.newReload
  , sessions:     Sessions.empty
  , showCorpus:   false
  , showLogin:    false
  }

type Cursors =
  { handed       :: T.Cursor Handed
  , forestOpen   :: T.Cursor OpenNodes
  , reloadRoot   :: T.Cursor Int
  , reloadForest :: T.Cursor Int
  , route        :: T.Cursor AppRoute
  , sessions     :: T.Cursor Sessions
  , showCorpus   :: T.Cursor Boolean
  , showLogin    :: T.Cursor Boolean
  }

