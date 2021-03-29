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
  , forestOpen   :: OpenNodes
  , handed       :: Handed
  , reloadForest :: Int
  , reloadRoot   :: Int
  , route        :: AppRoute
  , sessions     :: Sessions
  , showCorpus   :: Boolean
  , showLogin    :: Boolean
  }

emptyApp :: App
emptyApp =
  { backend:      Nothing
  , forestOpen:   Set.empty
  , handed:       RightHanded
  , reloadForest: T2.newReload
  , reloadRoot:   T2.newReload
  , route:        Home
  , sessions:     Sessions.empty
  , showCorpus:   false
  , showLogin:    false
  }

type Boxes =
  { backend      :: T.Box (Maybe Backend)
  , forestOpen   :: T.Box OpenNodes
  , handed       :: T.Box Handed
  , reloadForest :: T.Box T2.Reload
  , reloadRoot   :: T.Box T2.Reload
  , route        :: T.Box AppRoute
  , sessions     :: T.Box Sessions
  , showCorpus   :: T.Box Boolean
  , showLogin    :: T.Box Boolean
  }

