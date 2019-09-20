-- TODO copy of Gargantext.Pages.Corpus.Tabs.Specs
module Gargantext.Pages.Annuaire.User.Contacts.Tabs.Specs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))

import Gargantext.Config (Ends, TabType(..), TabSubType(..), PTabNgramType(..), CTabNgramType(..))
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Pages.Annuaire.User.Contacts.Types (ContactData)
import Gargantext.Utils.Reactix as R2

import Reactix as R
import Reactix.DOM.HTML as H

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

type Props =
  ( nodeId :: Int
  , contactData :: ContactData
  , ends :: Ends )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "G.P.Annuaire.User.Contacts.Tabs.tabs" cpt
  where
    cpt {nodeId, contactData: {defaultListId}, ends} _ = do
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
            patentsView = {ends, defaultListId, nodeId, mode: Patents}
            booksView = {ends, defaultListId, nodeId, mode: Books}
            commView = {ends, defaultListId, nodeId, mode: Communication}
            chart = mempty
            totalRecords = 4736 -- TODO
            docs = DT.docView
              { ends, nodeId, chart, totalRecords
              , tabType: TabPairing TabDocs
              , listId: defaultListId
              , corpusId: Nothing
              , showSearch: true }


type NgramsViewProps =
  ( ends :: Ends
  , mode :: Mode
  , defaultListId :: Int
  , nodeId :: Int )

ngramsView :: Record NgramsViewProps -> R.Element
ngramsView {ends,mode, defaultListId, nodeId} =
  NT.mainNgramsTable
  { nodeId, defaultListId, tabType, ends, tabNgramType }
  where
    tabNgramType = modeTabType' mode
    tabType = TabPairing $ TabNgramType $ modeTabType mode
