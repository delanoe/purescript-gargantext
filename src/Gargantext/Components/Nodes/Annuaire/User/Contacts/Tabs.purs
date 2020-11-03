-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs where

import Prelude hiding (div)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Reactix as R

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData)
import Gargantext.Components.Nodes.Lists.Types as NTypes
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session)
import Gargantext.Types (TabType(..), TabSubType(..), CTabNgramType(..), PTabNgramType(..))
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs"


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
    asyncTasks    :: GAT.Reductor
  , cacheState :: R.State NTypes.CacheState
  , contactData :: ContactData
  , frontends :: Frontends
  , nodeId :: Int
  , session :: Session
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R.hooksComponentWithModule thisModule "tabs" cpt
  where
    cpt { asyncTasks, cacheState, contactData: {defaultListId}, frontends, nodeId, session} _ = do
      active <- R.useState' 0
      pure $
        Tab.tabs { selected: fst active, tabs: tabs' }
      where
        tabs' =
          [ "Documents"     /\ docs
          , "Patents"       /\ ngramsView patentsView
          , "Books"         /\ ngramsView booksView
          , "Communication" /\ ngramsView commView
          , "Trash"         /\ docs -- TODO pass-in trash mode
          ]
          where
            patentsView = { asyncTasks, cacheState, defaultListId, mode: Patents, nodeId, session }
            booksView   = { asyncTasks, cacheState, defaultListId, mode: Books, nodeId, session }
            commView    = { asyncTasks, cacheState, defaultListId, mode: Communication, nodeId, session }
            chart       = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docViewLayout
              { frontends, session, nodeId, chart, totalRecords
              , tabType: TabPairing TabDocs
              , listId: defaultListId
              , corpusId: Nothing
              , showSearch: true }


type NgramsViewTabsProps = (
    asyncTasks    :: GAT.Reductor
  , cacheState :: R.State NTypes.CacheState
  , defaultListId :: Int
  , mode :: Mode
  , nodeId :: Int
  , session :: Session
  )

ngramsView :: Record NgramsViewTabsProps -> R.Element
ngramsView { asyncTasks, cacheState, defaultListId, mode, nodeId, session } =
  NT.mainNgramsTable {
      afterSync: \_ -> pure unit
    , asyncTasks
    , cacheState
    , defaultListId
    , nodeId
    , tabType
    , session
    , tabNgramType
    , withAutoUpdate: false
    }
  where
    tabNgramType = modeTabType' mode
    tabType      = TabPairing $ TabNgramType $ modeTabType mode
