-- TODO copy of Gargantext.Pages.Corpus.Tabs.Specs
module Gargantext.Pages.Annuaire.User.Contacts.Tabs.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Annuaire.User.Contacts.Types (Props)
import Gargantext.Pages.Annuaire.User.Contacts.Tabs.Documents as DV
import Gargantext.Pages.Annuaire.User.Contacts.Tabs.Ngrams.NgramsTable as NV
import Gargantext.Components.Tab as Tab
import Thermite (Spec, focus, hideState, noState, cmapProps)

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"     $ noState DV.docViewSpec
    , Tuple "Patents"       $ ngramsViewSpec {mode: NV.Patents}
    , Tuple "Books"         $ ngramsViewSpec {mode: NV.Books}
    , Tuple "Communication" $ ngramsViewSpec {mode: NV.Communication}
    , Tuple "Trash"         $ noState DV.docViewSpec -- TODO pass-in trash mode
    ]

ngramsViewSpec :: {mode :: NV.Mode} -> Spec Tab.State Props Tab.Action
ngramsViewSpec {mode} =
  cmapProps (\{loaded, path, dispatch} -> {mode,loaded,path, dispatch})
            (noState NV.ngramsTableSpec)
