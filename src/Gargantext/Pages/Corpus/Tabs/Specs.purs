module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Corpus.Tabs.Types (Props)
import Gargantext.Pages.Corpus.Tabs.States (State(), _doclens, _sourcelens, _authorlens, _termslens, _tablens, initialState, _trashlens)
import Gargantext.Pages.Corpus.Tabs.Actions (Action(), _docAction, _sourceAction, _authorAction, _termsAction, _tabAction, _trashAction)

import Gargantext.Pages.Corpus.Tabs.Documents as DV
import Gargantext.Pages.Corpus.Tabs.Sources   as SV
import Gargantext.Pages.Corpus.Tabs.Authors   as AV
import Gargantext.Pages.Corpus.Tabs.Terms     as TV
import Gargantext.Pages.Corpus.Tabs.Trash     as TT
import Gargantext.Components.Tab              as Tab
import Thermite (Spec, focus, hideState, cmapProps)


pureTabs :: Spec {} Props Void
pureTabs = hideState initialState statefulTabs

statefulTabs :: Spec State Props Action
statefulTabs =
  Tab.tabs _tablens _tabAction $ fromFoldable [ Tuple "Documents" docPageSpec
                                              , Tuple "Authors"   authorPageSpec
                                              , Tuple "Sources"   sourcePageSpec
                                              , Tuple "Terms"     termsPageSpec
                                              , Tuple "Trash"     trashPageSpec
                                              ]

docPageSpec :: Spec State Props Action
docPageSpec = focus _doclens _docAction DV.layoutDocview

authorPageSpec :: Spec State  Props Action
authorPageSpec = cmapProps (const {}) (focus _authorlens _authorAction AV.authorSpec)
              <> docPageSpec

sourcePageSpec :: Spec State Props Action
sourcePageSpec = cmapProps (const {}) (focus _sourcelens _sourceAction SV.sourceSpec)
              <> docPageSpec

termsPageSpec :: Spec State Props Action
termsPageSpec = cmapProps (const {}) (focus _termslens _termsAction TV.termsSpec)
             <> docPageSpec

trashPageSpec :: Spec State Props Action
trashPageSpec = cmapProps (const {}) (focus _trashlens _trashAction TT.spec)
              <> docPageSpec


