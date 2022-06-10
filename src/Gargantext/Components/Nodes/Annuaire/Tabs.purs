-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.Tabs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.DocsTable.Types (Year)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Lists.Types as LTypes
import Gargantext.Components.Nodes.Texts.Types as TextsT
import Gargantext.Components.Tab as Tab
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), PTabNgramType(..), TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Record as Record
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs"

data Mode = Patents | Books | Communication

derive instance Generic Mode _

instance Show Mode where
  show = genericShow

derive instance Eq Mode

modeTabType :: Mode -> PTabNgramType
modeTabType Patents = PTabPatents
modeTabType Books = PTabBooks
modeTabType Communication = PTabCommunication

-- TODO fix this type
modeTabType' :: Mode -> CTabNgramType
modeTabType' Patents = CTabAuthors
modeTabType' Books = CTabAuthors
modeTabType' Communication = CTabAuthors

type TabsProps =
  ( boxes         :: Boxes
  , cacheState    :: T.Box LTypes.CacheState
  , defaultListId :: Int
  , frontends     :: Frontends
  , nodeId        :: Int
  , session       :: Session
  , sidePanel     :: T.Box (Maybe (Record TextsT.SidePanel))
  )

tabs :: R2.Leaf TabsProps
tabs = R2.leafComponent tabsCpt
tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt where
  cpt props _ = do
    activeTab <- T.useBox 0
    yearFilter <- T.useBox (Nothing :: Maybe Year)
    chartReload <- T.useBox T2.newReload

    pure $ Tab.tabs { activeTab, tabs: tabs' yearFilter chartReload props }
  tabs' yearFilter chartReload props@{ boxes, defaultListId, sidePanel } =
    [ "Documents"     /\ docs
    , "Patents"       /\ ngramsView (viewProps Patents)
    , "Books"         /\ ngramsView (viewProps Books)
    , "Communication" /\ ngramsView (viewProps Communication)
    , "Trash"         /\ docs -- TODO pass-in trash mode
    ] where
      viewProps mode = Record.merge props { mode }
      totalRecords = 4736  -- TODO lol
      docs = DT.docViewLayout (Record.merge { boxes, chartReload, sidePanel } $ Record.merge dtCommon dtExtra)
      dtCommon = RX.pick props :: Record DTCommon
      dtExtra =
        { chart: mempty
        --, listId: props.contactData.defaultListId
        , listId: defaultListId
        , mCorpusId: Nothing
        , showSearch: true
        , tabType: TabPairing TabDocs
        , totalRecords
        , yearFilter
        }

type DTCommon =
  ( cacheState        :: T.Box LTypes.CacheState
  -- , contactData       :: ContactData
  , frontends         :: Frontends
  , nodeId            :: Int
  , session           :: Session
  -- , sidePanel    :: T.Box (Record SidePanel)
  )

type NgramsViewTabsProps =
  ( mode          :: Mode
  | TabsProps )

ngramsView :: R2.Leaf NgramsViewTabsProps
ngramsView = R2.leafComponent ngramsViewCpt
ngramsViewCpt :: R.Component NgramsViewTabsProps
ngramsViewCpt = here.component "ngramsView" cpt where
  cpt props@{ defaultListId, mode, nodeId, session } _ = do
    path <- T.useBox $
      NTC.initialPageParams session nodeId
      [ defaultListId ] (TabDocument TabDocs)

    treeEdit <- T.useBox NT.initialTreeEdit

    pure $ NT.mainNgramsTable (props' treeEdit path) []
      where
        most = RX.pick props :: Record NTCommon
        props' treeEdit path =
          (Record.merge most
           { afterSync
           , path
           , tabType:        TabPairing (TabNgramType $ modeTabType mode)
           , tabNgramType:   modeTabType' mode
           , treeEdit
           , withAutoUpdate: false }) :: Record NT.MainNgramsTableProps
        afterSync :: Unit -> Aff Unit
        afterSync _ = pure unit

type NTCommon =
  ( boxes         :: Boxes
  , cacheState    :: T.Box LTypes.CacheState
  , defaultListId :: Int
  , session       :: Session
  )
