-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.DocsTable.Types (Year)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData')
import Gargantext.Components.Nodes.Lists.Types as LTypes
import Gargantext.Components.Nodes.Texts.Types as TTypes
import Gargantext.Components.Tab as Tab
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), FrontendError, PTabNgramType(..), SidePanelState, TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
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

type TabsProps = (
    boxes       :: Boxes
  , cacheState  :: T.Box LTypes.CacheState
  , contactData :: ContactData'
  , frontends   :: Frontends
  , nodeId      :: Int
  , session     :: Session
  , sidePanel   :: T.Box (Maybe (Record TTypes.SidePanel))
  )

tabs :: R2.Leaf TabsProps
tabs props = R.createElement tabsCpt props []
tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt
  where
    cpt { boxes
        , cacheState
        , contactData: {defaultListId}
        , frontends
        , nodeId
        , session
        , sidePanel
        } _ = do
      activeTab <- T.useBox 0
      yearFilter <- T.useBox (Nothing :: Maybe Year)

      pure $ Tab.tabs { activeTab, tabs: tabs' yearFilter }
      where
        tabs' yearFilter =
          [ "Documents"     /\ docs
          , "Patents"       /\ ngramsView patentsView []
          , "Books"         /\ ngramsView booksView []
          , "Communication" /\ ngramsView commView []
          , "Trash"         /\ docs -- TODO pass-in trash mode
          ]
          where
            patentsView = { boxes
                          , cacheState
                          , defaultListId
                          , mode: Patents
                          , nodeId
                          , session
                          }
            booksView   = { boxes
                          , cacheState
                          , defaultListId
                          , mode: Books
                          , nodeId
                          , session
                          }
            commView    = { boxes
                          , cacheState
                          , defaultListId
                          , mode: Communication
                          , nodeId
                          , session
                          }
            chart       = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docViewLayout
              { boxes
              , cacheState
              , chart
              , frontends
              , listId: defaultListId
              , mCorpusId: Nothing
              , nodeId
              , session
              , showSearch: true
              , sidePanel
              , tabType: TabPairing TabDocs
              , totalRecords
              , yearFilter
              }


type NgramsViewTabsProps = (
    boxes          :: Boxes
  , cacheState     :: T.Box LTypes.CacheState
  , defaultListId  :: Int
  , mode           :: Mode
  , nodeId         :: Int
  , session        :: Session
  )

ngramsView :: R2.Component NgramsViewTabsProps
ngramsView = R.createElement ngramsViewCpt

ngramsViewCpt :: R.Component NgramsViewTabsProps
ngramsViewCpt = here.component "ngramsView" cpt
  where
    cpt { boxes
        , cacheState
        , defaultListId
        , mode
        , nodeId
        , session } _ = do
      path <- T.useBox $ NTC.initialPageParams session nodeId [defaultListId] (TabDocument TabDocs)

      pure $ NT.mainNgramsTable {
          afterSync: \_ -> pure unit
        , boxes
        , cacheState
        , defaultListId
        , path
        , session
        , tabNgramType
        , tabType
        , withAutoUpdate: false
        } []
      where
        tabNgramType = modeTabType' mode
        tabType      = TabPairing $ TabNgramType $ modeTabType mode
