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
    appReload     :: R.State Int
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , cacheState    :: R.State NTypes.CacheState
  , contactData   :: ContactData
  , frontends     :: Frontends
  , nodeId        :: Int
  , session       :: Session
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R.hooksComponentWithModule thisModule "tabs" cpt
  where
    cpt { appReload, asyncTasksRef, cacheState, contactData: {defaultListId}, frontends, nodeId, session} _ = do
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
            patentsView = { appReload, asyncTasksRef, cacheState, defaultListId, mode: Patents, nodeId, session }
            booksView   = { appReload, asyncTasksRef, cacheState, defaultListId, mode: Books, nodeId, session }
            commView    = { appReload, asyncTasksRef, cacheState, defaultListId, mode: Communication, nodeId, session }
            chart       = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docViewLayout
              { cacheState
              , chart
              , corpusId: Nothing
              , frontends
              , listId: defaultListId
              , nodeId
              , session
              , showSearch: true
              , tabType: TabPairing TabDocs
              , totalRecords
              }


type NgramsViewTabsProps = (
    appReload     :: R.State Int
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , cacheState    :: R.State NTypes.CacheState
  , defaultListId :: Int
  , mode          :: Mode
  , nodeId        :: Int
  , session       :: Session
  )

ngramsView :: Record NgramsViewTabsProps -> R.Element
ngramsView { appReload, asyncTasksRef, cacheState, defaultListId, mode, nodeId, session } =
  NT.mainNgramsTable {
      appReload
    , afterSync: \_ -> pure unit
    , asyncTasksRef
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
