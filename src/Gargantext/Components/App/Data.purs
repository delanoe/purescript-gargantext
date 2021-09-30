module Gargantext.Components.App.Data (App, Boxes, emptyApp) where


import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.Nodes.Lists.Types as ListsT
import Gargantext.Components.Nodes.Texts.Types as TextsT
import Gargantext.Components.Themes as Themes
import Gargantext.Ends (Backend)
import Gargantext.Routes (AppRoute(Home), Tile)
import Gargantext.Sessions (Session, Sessions)
import Gargantext.Sessions as Sessions
import Gargantext.Sessions.Types (OpenNodes(..))
import Gargantext.Types (FrontendError, Handed(RightHanded), SidePanelState(..))
import Gargantext.Utils.Toestand as T2
import Toestand as T

type App =
  { backend             :: Maybe Backend
  , errors              :: Array FrontendError
  , forestOpen          :: OpenNodes
  , graphVersion        :: T2.Reload
  , handed              :: Handed
  , reloadForest        :: T2.Reload
  , reloadMainPage      :: T2.Reload
  , reloadRoot          :: T2.Reload
  , route               :: AppRoute
  , session             :: Maybe Session
  , sessions            :: Sessions
  , showCorpus          :: Boolean
  , showLogin           :: Boolean
  , showTree            :: Boolean
  , sidePanelGraph      :: Maybe (Record GEST.SidePanel)
  , sidePanelLists      :: Maybe (Record ListsT.SidePanel)
  , sidePanelTexts      :: Maybe (Record TextsT.SidePanel)
  , sidePanelState      :: SidePanelState
  , tasks               :: GAT.Storage
  , theme               :: Themes.Theme
  , tileAxisXList       :: Array (Record Tile)
  , tileAxisYList       :: Array (Record Tile)
  }

emptyApp :: App
emptyApp =
  { backend             : Nothing
  , errors              : []
  , forestOpen          : OpenNodes $ Set.empty
  , graphVersion        : T2.newReload
  , handed              : RightHanded
  , reloadForest        : T2.newReload
  , reloadMainPage      : T2.newReload
  , reloadRoot          : T2.newReload
  , route               : Home
  , session             : Nothing
  , sessions            : Sessions.empty
  , showCorpus          : false
  , showLogin           : false
  , showTree            : true
  , sidePanelGraph      : GEST.initialSidePanel
  , sidePanelLists      : ListsT.initialSidePanel
  , sidePanelTexts      : TextsT.initialSidePanel
  , sidePanelState      : InitialClosed
  , tasks               : GAT.empty
  , theme               : Themes.defaultTheme
  , tileAxisXList       : mempty
  , tileAxisYList       : mempty
  }

type Boxes =
  { backend             :: T.Box (Maybe Backend)
  , errors              :: T.Box (Array FrontendError)
  , forestOpen          :: T.Box OpenNodes
  , graphVersion        :: T2.ReloadS
  , handed              :: T.Box Handed
  , reloadForest        :: T2.ReloadS
  , reloadMainPage      :: T2.ReloadS
  , reloadRoot          :: T2.ReloadS
  , route               :: T.Box AppRoute
  , session             :: T.Box (Maybe Session)
  , sessions            :: T.Box Sessions
  , showCorpus          :: T.Box Boolean
  , showLogin           :: T.Box Boolean
  , showTree            :: T.Box Boolean
  , sidePanelGraph      :: T.Box (Maybe (Record GEST.SidePanel))
  , sidePanelLists      :: T.Box (Maybe (Record ListsT.SidePanel))
  , sidePanelTexts      :: T.Box (Maybe (Record TextsT.SidePanel))
  , sidePanelState      :: T.Box SidePanelState
  , tasks               :: T.Box GAT.Storage
  , theme               :: T.Box Themes.Theme
  , tileAxisXList       :: T.Box (Array (Record Tile))
  , tileAxisYList       :: T.Box (Array (Record Tile))
  }
