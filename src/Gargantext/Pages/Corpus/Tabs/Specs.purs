module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Corpus.Tabs.States (State(), _doclens, _sourcelens, _authorlens, _termslens, _tablens, initialState)
import Gargantext.Pages.Corpus.Tabs.Actions (Action(), _docAction, _sourceAction, _authorAction, _termsAction, _tabAction)

import Gargantext.Pages.Corpus.Tabs.Documents as DV
import Gargantext.Pages.Corpus.Tabs.Sources as SV
import Gargantext.Pages.Corpus.Tabs.Authors as AV
import Gargantext.Pages.Corpus.Tabs.Terms as TV
import Gargantext.Components.Tab as Tab
import Thermite (Spec, focus, hideState)


pureTabs :: Spec {} {} Void
pureTabs = hideState initialState statefulTabs

statefulTabs :: Spec State {} Action
statefulTabs =
  Tab.tabs _tablens _tabAction $ fromFoldable [ Tuple "Documents" docPageSpec
                                              , Tuple "Authors"   authorPageSpec
                                              , Tuple "Sources"   sourcePageSpec
                                              , Tuple "Terms"     termsPageSpec
                                              ]

docPageSpec :: Spec State {} Action
docPageSpec = focus _doclens _docAction DV.layoutDocview

authorPageSpec :: Spec State  {} Action
authorPageSpec = focus _authorlens _authorAction AV.authorspec'

sourcePageSpec :: Spec State {} Action
sourcePageSpec = focus _sourcelens _sourceAction SV.sourcespec'

termsPageSpec :: Spec State {} Action
termsPageSpec = focus _termslens _termsAction TV.termSpec'
