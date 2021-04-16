module Gargantext.Components.App.Data (App, Boxes, emptyApp) where

import Data.Set as Set
import Data.Maybe (Maybe(..))
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.Nodes.Lists.Types as ListsT
import Gargantext.Components.Nodes.Texts.Types as TextsT
import Gargantext.Ends (Backend)
import Gargantext.Routes (AppRoute(Home))
import Gargantext.Sessions as Sessions
import Gargantext.Sessions (OpenNodes, Sessions)
import Gargantext.Types (Handed(RightHanded), SidePanelState(..))
import Gargantext.Utils.Toestand as T2

type App =
  { backend                 :: Maybe Backend
  , forestOpen              :: OpenNodes
  , graphVersion            :: T2.Reload
  , handed                  :: Handed
  , reloadForest            :: Int
  , reloadRoot              :: Int
  , route                   :: AppRoute
  , sessions                :: Sessions
  , showCorpus              :: Boolean
  , showLogin               :: Boolean
  , showTree                :: Boolean
  , sidePanelGraph          :: Maybe (Record GEST.SidePanel)
  , sidePanelLists          :: Maybe (Record ListsT.SidePanel)
  , sidePanelTexts          :: Maybe (Record TextsT.SidePanel)
  , sidePanelState          :: SidePanelState
  , tasks                   :: GAT.Storage
  }

emptyApp :: App
emptyApp =
  { backend                 : Nothing
  , forestOpen              : Set.empty
  , graphVersion            : T2.newReload
  , handed                  : RightHanded
  , reloadForest            : T2.newReload
  , reloadRoot              : T2.newReload
  , route                   : Home
  , sessions                : Sessions.empty
  , showCorpus              : false
  , showLogin               : false
  , showTree                : true
  , sidePanelGraph          : GEST.initialSidePanel
  , sidePanelLists          : ListsT.initialSidePanel
  , sidePanelTexts          : TextsT.initialSidePanel
  , sidePanelState          : InitialClosed
  , tasks                   : GAT.empty
  }

type Boxes =
  { backend                 :: T.Box (Maybe Backend)
  , forestOpen              :: T.Box OpenNodes
  , graphVersion            :: T2.ReloadS
  , handed                  :: T.Box Handed
  , reloadForest            :: T.Box T2.Reload
  , reloadRoot              :: T.Box T2.Reload
  , route                   :: T.Box AppRoute
  , sessions                :: T.Box Sessions
  , showCorpus              :: T.Box Boolean
  , showLogin               :: T.Box Boolean
  , showTree                :: T.Box Boolean
  , sidePanelGraph          :: T.Box (Maybe (Record GEST.SidePanel))
  , sidePanelLists          :: T.Box (Maybe (Record ListsT.SidePanel))
  , sidePanelTexts          :: T.Box (Maybe (Record TextsT.SidePanel))
  , sidePanelState          :: T.Box SidePanelState
  , tasks                   :: T.Box GAT.Storage
  }
