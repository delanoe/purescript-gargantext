module Gargantext.Components.App.Data (App, Boxes, emptyApp) where

import Data.Set as Set
import Data.Maybe (Maybe(..))
import Toestand as T

import Gargantext.Ends (Backend)
import Gargantext.Sessions as Sessions
import Gargantext.Sessions (OpenNodes, Sessions)
import Gargantext.Routes (AppRoute(Home))
import Gargantext.Types (Handed(RightHanded))
import Gargantext.Utils.Toestand as T2

type App =
  { backend      :: Maybe Backend
  , handed       :: Handed
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
  { backend:      Nothing
  , handed:       RightHanded
  , route:        Home
  , forestOpen:   Set.empty
  , reloadRoot:   T2.newReload
  , reloadForest: T2.newReload
  , sessions:     Sessions.empty
  , showCorpus:   false
  , showLogin:    false
  }

type Boxes =
  { backend      :: T.Box (Maybe Backend)
  , handed       :: T.Box Handed
  , forestOpen   :: T.Box OpenNodes
  , reloadRoot   :: T.Box T2.Reload
  , reloadForest :: T.Box T2.Reload
  , route        :: T.Box AppRoute
  , sessions     :: T.Box Sessions
  , showCorpus   :: T.Box Boolean
  , showLogin    :: T.Box Boolean
  }

