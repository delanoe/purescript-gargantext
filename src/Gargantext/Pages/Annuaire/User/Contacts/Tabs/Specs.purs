-- TODO copy of Gargantext.Pages.Corpus.Tabs.Specs
module Gargantext.Pages.Annuaire.User.Contacts.Tabs.Specs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Gargantext.Config (TabType(..), TabSubType(..), PTabNgramType(..), CTabNgramType(..))
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Pages.Annuaire.User.Contacts.Types (ContactData)

import React (Children, ReactElement, ReactClass, createElement)
import Thermite (Spec, focus, hideState, noState, cmapProps, createClass)

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



type PropsRow =
  ( nodeId :: Int
  , contactData :: ContactData
  )

type Props = Record PropsRow

elt :: Props -> ReactElement
elt props = createElement tabsClass props []

tabsClass :: ReactClass { children :: Children | PropsRow }
tabsClass = createClass "ContactsTabs" pureTabs (const {})

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"     $ docs
    , Tuple "Patents"       $ ngramsViewSpec {mode: Patents}
    , Tuple "Books"         $ ngramsViewSpec {mode: Books}
    , Tuple "Communication" $ ngramsViewSpec {mode: Communication}
    , Tuple "Trash"         $ docs -- TODO pass-in trash mode
    ]
  where
    chart = mempty
    -- TODO totalRecords
    docs = cmapProps (\{nodeId, contactData: {defaultListId}} ->
                       { nodeId, chart
                       , tabType: TabPairing TabDocs
                       , totalRecords: 4736
                       , listId: defaultListId
                       , corpusId: Nothing}) $
           noState DT.docViewSpec

ngramsViewSpec :: {mode :: Mode} -> Spec Tab.State Props Tab.Action
ngramsViewSpec {mode} =
  cmapProps (\{contactData: {defaultListId}, nodeId} ->
              {defaultListId, nodeId, tabType})
            (noState (NT.mainNgramsTableSpec (modeTabType' mode)))
    where
      tabType = TabPairing $ TabNgramType $ modeTabType mode
