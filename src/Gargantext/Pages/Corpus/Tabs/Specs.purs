module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Corpus.Tabs.Types (Props)
import Gargantext.Pages.Corpus.Tabs.States (State(), _doclens, _ngramsView, _tablens, initialState)
import Gargantext.Pages.Corpus.Tabs.Actions (Action(), _docAction, _NgramViewA, _tabAction)

import Gargantext.Pages.Corpus.Tabs.Documents as DV
import Gargantext.Pages.Corpus.Tabs.Ngrams as NV
import Gargantext.Components.Tab as Tab
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

ngramsViewSpec :: NV.Props -> Spec State Props Action
ngramsViewSpec props = cmapProps (const props) (focus _ngramsView _NgramViewA NV.ngramsSpec)

authorPageSpec :: Spec State Props Action
authorPageSpec = ngramsViewSpec {mode: NV.Authors}

sourcePageSpec :: Spec State Props Action
sourcePageSpec = ngramsViewSpec {mode: NV.Sources}

termsPageSpec :: Spec State Props Action
termsPageSpec = ngramsViewSpec {mode: NV.Terms}

trashPageSpec :: Spec State Props Action
trashPageSpec = ngramsViewSpec {mode: NV.Trash}
