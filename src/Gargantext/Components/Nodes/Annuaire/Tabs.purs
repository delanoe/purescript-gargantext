-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Tabs where

import Prelude hiding (div)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R

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
import Gargantext.Types (CTabNgramType(..), NodeID, PTabNgramType(..), TabType(..), TabSubType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

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
    appReload         :: GUR.ReloadS
  , asyncTasksRef     :: R.Ref (Maybe GAT.Reductor)
  , cacheState        :: R.State LTypes.CacheState
  , contactData       :: ContactData
  , frontends         :: Frontends
  , nodeId            :: Int
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  , treeReloadRef     :: GUR.ReloadWithInitializeRef
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R.hooksComponentWithModule thisModule "tabs" cpt
  where
    cpt { appReload
        , asyncTasksRef
        , cacheState
        , contactData: {defaultListId}
        , frontends
        , nodeId
        , session
        , sidePanelTriggers
        , treeReloadRef } _ = do
      active <- R.useState' 0
      textsSidePanelTriggers <- TTypes.emptySidePanelTriggers
      pure $ Tab.tabs { selected: fst active, tabs: tabs' textsSidePanelTriggers }
      where
        tabs' trg =
          [ "Documents"     /\ docs trg
          , "Patents"       /\ ngramsView patentsView []
          , "Books"         /\ ngramsView booksView []
          , "Communication" /\ ngramsView commView []
          , "Trash"         /\ docs trg -- TODO pass-in trash mode
          ]
          where
            patentsView = { appReload
                          , asyncTasksRef
                          , cacheState
                          , defaultListId
                          , mode: Patents
                          , nodeId
                          , session
                          , sidePanelTriggers
                          , treeReloadRef }
            booksView   = { appReload
                          , asyncTasksRef
                          , cacheState
                          , defaultListId
                          , mode: Books
                          , nodeId
                          , session
                          , sidePanelTriggers
                          , treeReloadRef }
            commView    = { appReload, asyncTasksRef
                          , cacheState
                          , defaultListId
                          , mode: Communication
                          , nodeId
                          , session
                          , sidePanelTriggers
                          , treeReloadRef }
            chart       = mempty
            totalRecords = 4736 -- TODO
            docs sidePanelTriggers = DT.docViewLayout
              { cacheState
              , chart
              , frontends
              , listId: defaultListId
              , mCorpusId: Nothing
              , nodeId
              , session
              , showSearch: true
              , sidePanelTriggers
              , tabType: TabPairing TabDocs
              , totalRecords
              }


type NgramsViewTabsProps = (
    appReload         :: GUR.ReloadS
  , asyncTasksRef     :: R.Ref (Maybe GAT.Reductor)
  , cacheState        :: R.State LTypes.CacheState
  , defaultListId     :: Int
  , mode              :: Mode
  , nodeId            :: Int
  , session           :: Session
  , sidePanelTriggers :: Record LTypes.SidePanelTriggers
  , treeReloadRef     :: GUR.ReloadWithInitializeRef
  )

ngramsView :: R2.Component NgramsViewTabsProps
ngramsView = R.createElement ngramsViewCpt

ngramsViewCpt :: R.Component NgramsViewTabsProps
ngramsViewCpt = R.hooksComponentWithModule thisModule "ngramsView" cpt
  where
    cpt { appReload
        , asyncTasksRef
        , cacheState
        , defaultListId
        , mode
        , nodeId
        , session
        , sidePanelTriggers
        , treeReloadRef } _ = do
      let path = NTC.initialPageParams session nodeId [defaultListId] (TabDocument TabDocs)

      pure $ NT.mainNgramsTable {
          appReload
        , afterSync: \_ -> pure unit
        , asyncTasksRef
        , cacheState
        , defaultListId
        , path
        , sidePanelTriggers
        , tabNgramType
        , treeReloadRef
        , withAutoUpdate: false
        } []
      where
        tabNgramType = modeTabType' mode
        tabType      = TabPairing $ TabNgramType $ modeTabType mode
