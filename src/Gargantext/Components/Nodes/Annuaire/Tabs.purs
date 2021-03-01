-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Tabs where

import Prelude hiding (div)
import Effect.Aff (Aff)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Record as Record
import Record.Extra as RX
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData)
import Gargantext.Components.Nodes.Lists.Types as LTypes
import Gargantext.Components.Nodes.Texts.Types as TTypes
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), PTabNgramType(..), TabType(..), TabSubType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs"

data Mode = Patents | Books | Communication

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

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
  ( tasks             :: R.Ref (Maybe GAT.Reductor)
  , cacheState        :: R.State LTypes.CacheState
  , contactData       :: ContactData
  , frontends         :: Frontends
  , nodeId            :: Int
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  , reloadForest      :: T.Cursor (T2.InitReload T.Cursor)
  , reloadRoot        :: T.Cursor T2.Reload
  )

type NgramsViewTabsProps =
  ( mode          :: Mode
  , defaultListId :: Int
  | TabsProps )

tabs :: R2.Leaf TabsProps
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt where
  cpt props _ = do
    active <- R.useState' 0
    triggers <- TTypes.emptySidePanelTriggers
    pure $ Tab.tabs { selected: fst active, tabs: tabs' props triggers }
  tabs' props trg =
    [ "Documents"     /\ docs trg
    , "Patents"       /\ ngramsView (viewProps Patents)
    , "Books"         /\ ngramsView (viewProps Books)
    , "Communication" /\ ngramsView (viewProps Communication)
    , "Trash"         /\ docs trg -- TODO pass-in trash mode
    ] where
      viewProps mode = Record.merge dtCommon { mode }
      totalRecords = 4736 -- TODO lol
      docs sidePanelTriggers = DT.docViewLayout (Record.merge dtCommon dtExtra)
      dtCommon = RX.pick props :: Record DTCommon
      dtExtra = 
        { chart: mempty, mCorpusId: Nothing, showSearch: true
        , listId: props.contactData.defaultListId
        , tabType: TabPairing TabDocs, totalRecords
        }

type DTCommon =
  ( cacheState        :: R.State LTypes.CacheState
  , contactData       :: ContactData
  , frontends         :: Frontends
  , nodeId            :: Int
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  )

ngramsView :: R2.Leaf NgramsViewTabsProps
ngramsView props = R.createElement ngramsViewCpt props []

ngramsViewCpt :: R.Component NgramsViewTabsProps
ngramsViewCpt = here.component "ngramsView" cpt where
  cpt props@{ defaultListId, nodeId, session, mode } _ = do
    path <- R.useState' $
      NTC.initialPageParams session nodeId
      [ defaultListId ] (TabDocument TabDocs)
    pure $ NT.mainNgramsTable (props' path) [] where
      most = RX.pick props :: Record NTCommon
      props' path =
        Record.merge most
        { tabType:        TabPairing (TabNgramType $ modeTabType mode)
        , tabNgramType:   modeTabType' mode
        , withAutoUpdate: false
        , afterSync, path } where
          afterSync :: Unit -> Aff Unit
          afterSync _ = pure unit

type NTCommon =
  ( tasks             :: R.Ref (Maybe GAT.Reductor)
  , cacheState        :: R.State LTypes.CacheState
  , nodeId            :: Int
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  , reloadForest      :: T.Cursor (T2.InitReload T.Cursor)
  , reloadRoot        :: T.Cursor T2.Reload
  , defaultListId :: Int
  )
