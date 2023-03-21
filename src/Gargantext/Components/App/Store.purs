module Gargantext.Components.App.Store
  ( Store
  , State
  , options
  , context
  , provide
  , use
  -- legacy
  , Boxes
  ) where


import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Lang as Lang
import Gargantext.Components.Login.Types (TreeId)
import Gargantext.Components.Nodes.Lists.Types as ListsT
import Gargantext.Components.Nodes.Texts.Types as TextsT
import Gargantext.Components.Themes as Themes
import Gargantext.Ends (Backend)
import Gargantext.Routes (AppRoute(Home), Tile)
import Gargantext.Sessions (Session, Sessions)
import Gargantext.Sessions as Sessions
import Gargantext.Sessions.Types (OpenNodes(..))
import Gargantext.Types (FrontendError, Handed(RightHanded), SidePanelState(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Stores as Stores
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

here :: R2.Here
here = R2.here "Gargantext.Components.App.Store"

type Store =
  ( backend             :: T.Box (Maybe Backend)
  , errors              :: T.Box (Array FrontendError)
  , expandTableEdition  :: T.Box Boolean
  , forestOpen          :: T.Box OpenNodes
  , graphVersion        :: T2.ReloadS
  , handed              :: T.Box Handed
  , lang                :: T.Box Lang.LandingLang
  , reloadForest        :: T2.ReloadS
  , reloadMainPage      :: T2.ReloadS
  , reloadRoot          :: T2.ReloadS
  , route               :: T.Box AppRoute
  , session             :: T.Box (Maybe Session)
  , sessions            :: T.Box Sessions
  , showCorpus          :: T.Box Boolean
  , showLogin           :: T.Box Boolean
  , showTree            :: T.Box Boolean
  , sidePanelLists      :: T.Box (Maybe (Record ListsT.SidePanel))
  , sidePanelTexts      :: T.Box (Maybe (Record TextsT.SidePanel))
  , sidePanelState      :: T.Box SidePanelState
  , tasks               :: T.Box GAT.Storage
  , theme               :: T.Box Themes.Theme
  , tileAxisXList       :: T.Box (Array (Record Tile))
  , tileAxisYList       :: T.Box (Array (Record Tile))
  , pinnedTreeId        :: T.Box (Map String Int)
  )

type State =
  ( backend             :: Maybe Backend
  , errors              :: Array FrontendError
  , expandTableEdition  :: Boolean
  , forestOpen          :: OpenNodes
  , graphVersion        :: T2.Reload
  , handed              :: Handed
  , lang                :: Lang.LandingLang
  , reloadForest        :: T2.Reload
  , reloadMainPage      :: T2.Reload
  , reloadRoot          :: T2.Reload
  , route               :: AppRoute
  , session             :: Maybe Session
  , sessions            :: Sessions
  , showCorpus          :: Boolean
  , showLogin           :: Boolean
  , showTree            :: Boolean
  , sidePanelLists      :: Maybe (Record ListsT.SidePanel)
  , sidePanelTexts      :: Maybe (Record TextsT.SidePanel)
  , sidePanelState      :: SidePanelState
  , tasks               :: GAT.Storage
  , theme               :: Themes.Theme
  , tileAxisXList       :: Array (Record Tile)
  , tileAxisYList       :: Array (Record Tile)
  , pinnedTreeId        :: Map String Int
  )

options :: Record State
options =
  { backend             : Nothing
  , errors              : []
  , expandTableEdition  : false
  , forestOpen          : OpenNodes $ Set.empty
  , graphVersion        : T2.newReload
  , handed              : RightHanded
  , lang                : Lang.LL_EN
  , reloadForest        : T2.newReload
  , reloadMainPage      : T2.newReload
  , reloadRoot          : T2.newReload
  , route               : Home
  , session             : Nothing
  , sessions            : Sessions.empty
  , showCorpus          : false
  , showLogin           : false
  , showTree            : true
  , sidePanelLists      : ListsT.initialSidePanel
  , sidePanelTexts      : TextsT.initialSidePanel
  , sidePanelState      : InitialClosed
  , tasks               : GAT.empty
  , theme               : Themes.defaultTheme
  , tileAxisXList       : mempty
  , tileAxisYList       : mempty
  , pinnedTreeId        : Map.empty
  }

context :: R.Context (Record Store)
context = R.createContext $ unsafeCoerce unit

provide :: Record State -> Array R.Element -> R.Element
provide values = Stores.provideStore here.name values context

use :: R.Hooks (Record Store)
use = Stores.useStore context

------------------------------------------------------

type Boxes = Record Store
