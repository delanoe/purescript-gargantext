-- TODO copy of Gargantext.Components.Nodes.Corpus.Tabs.Specs
module Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs where

import Prelude hiding (div)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData)
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session)
import Gargantext.Types (TabType(..), TabSubType(..), CTabNgramType(..), PTabNgramType(..))


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
  ( nodeId :: Int
  , contactData :: ContactData
  , frontends :: Frontends
  , session :: Session )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R.hooksComponent "G.P.Annuaire.User.Contacts.Tabs.tabs" cpt
  where
    cpt {frontends, nodeId, contactData: {defaultListId}, session} _ = do
      active <- R.useState' 0
      pure $
        Tab.tabs { tabs: tabs', selected: fst active }
      where
        tabs' =
          [ "Documents"     /\ docs
          , "Patents"       /\ ngramsView patentsView
          , "Books"         /\ ngramsView booksView
          , "Communication" /\ ngramsView commView
          , "Trash"         /\ docs -- TODO pass-in trash mode
          ]
          where
            patentsView = {session, defaultListId, nodeId, mode: Patents}
            booksView = {session, defaultListId, nodeId, mode: Books}
            commView = {session, defaultListId, nodeId, mode: Communication}
            chart = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docViewLayout
              { frontends, session, nodeId, chart, totalRecords
              , tabType: TabPairing TabDocs
              , listId: defaultListId
              , corpusId: Nothing
              , showSearch: true }


type NgramsViewTabsProps =
  ( session :: Session
  , mode :: Mode
  , defaultListId :: Int
  , nodeId :: Int )

ngramsView :: Record NgramsViewTabsProps -> R.Element
ngramsView {session,mode, defaultListId, nodeId} =
  NT.mainNgramsTable
  { nodeId, defaultListId, tabType, session, tabNgramType }
  where
    tabNgramType = modeTabType' mode
    tabType = TabPairing $ TabNgramType $ modeTabType mode
