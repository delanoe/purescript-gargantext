-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs where

import Prelude hiding (div)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData')
import Gargantext.Components.Nodes.Lists.Types as LTypes
import Gargantext.Components.Nodes.Texts.Types as TTypes
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), PTabNgramType(..), SidePanelState, TabType(..), TabSubType(..))
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

type TabsProps = (
    cacheState     :: T.Box LTypes.CacheState
  , contactData    :: ContactData'
  , frontends      :: Frontends
  , nodeId         :: Int
  , reloadForest   :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  , session        :: Session
  , sidePanel      :: T.Box (Maybe (Record TTypes.SidePanel))
  , sidePanelState :: T.Box SidePanelState
  , tasks          :: T.Box GAT.Storage
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt
  where
    cpt { reloadRoot
        , tasks
        , cacheState
        , contactData: {defaultListId}
        , frontends
        , nodeId
        , session
        , sidePanel
        , sidePanelState
        , reloadForest } _ = do
      active <- R.useState' 0
      pure $ Tab.tabs { selected: fst active, tabs: tabs' }
      where
        tabs' =
          [ "Documents"     /\ docs
          , "Patents"       /\ ngramsView patentsView []
          , "Books"         /\ ngramsView booksView []
          , "Communication" /\ ngramsView commView []
          , "Trash"         /\ docs -- TODO pass-in trash mode
          ]
          where
            patentsView = { reloadRoot
                          , tasks
                          , cacheState
                          , defaultListId
                          , mode: Patents
                          , nodeId
                          , reloadForest
                          , session }
            booksView   = { reloadRoot
                          , tasks
                          , cacheState
                          , defaultListId
                          , mode: Books
                          , nodeId
                          , reloadForest
                          , session }
            commView    = { reloadRoot, tasks
                          , cacheState
                          , defaultListId
                          , mode: Communication
                          , nodeId
                          , reloadForest
                          , session }
            chart       = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docViewLayout
              { cacheState
              , chart
              , frontends
              , listId: defaultListId
              , mCorpusId: Nothing
              , nodeId
              , session
              , showSearch: true
              , sidePanel
              , sidePanelState
              , tabType: TabPairing TabDocs
              , totalRecords
              }


type NgramsViewTabsProps = (
    cacheState     :: T.Box LTypes.CacheState
  , defaultListId  :: Int
  , mode           :: Mode
  , nodeId         :: Int
  , reloadForest   :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  , session        :: Session
  , tasks          :: T.Box GAT.Storage
  )

ngramsView :: R2.Component NgramsViewTabsProps
ngramsView = R.createElement ngramsViewCpt

ngramsViewCpt :: R.Component NgramsViewTabsProps
ngramsViewCpt = here.component "ngramsView" cpt
  where
    cpt { cacheState
        , defaultListId
        , reloadForest
        , reloadRoot
        , mode
        , nodeId
        , session
        , tasks } _ = do
      path <- T.useBox $ NTC.initialPageParams session nodeId [defaultListId] (TabDocument TabDocs)

      pure $ NT.mainNgramsTable {
          afterSync: \_ -> pure unit
        , cacheState
        , defaultListId
        , path
        , reloadForest
        , reloadRoot
        , session
        , tabNgramType
        , tabType
        , tasks
        , withAutoUpdate: false
        } []
      where
        tabNgramType = modeTabType' mode
        tabType      = TabPairing $ TabNgramType $ modeTabType mode
