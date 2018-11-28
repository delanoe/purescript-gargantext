module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Pages.Corpus.Tabs.Types (Props)
import Gargantext.Pages.Corpus.Tabs.States (State(), initialState, _activeTab)
import Gargantext.Pages.Corpus.Tabs.Actions (Action(), _TabAction)

import Gargantext.Pages.Corpus.Tabs.Documents as DV
import Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable as NV
import Gargantext.Components.Tab as Tab
import Thermite (Spec, focus, hideState, noState, cmapProps)


-- pureTabs :: Spec {} Props Void
-- pureTabs = hideState initialState statefulTabs

statefulTabs :: Spec State Props Action
statefulTabs =
  Tab.tabs _activeTab _TabAction $ fromFoldable
    [ Tuple "Documents"  $ noState DV.docViewSpec
    , Tuple "Authors"    $ ngramsViewSpec {mode: NV.Authors}
    , Tuple "Sources"    $ ngramsViewSpec {mode: NV.Sources}
    , Tuple "Institutes" $ ngramsViewSpec {mode: NV.Institutes}
    , Tuple "Terms"      $ ngramsViewSpec {mode: NV.Terms}
    , Tuple "Trash"      $ noState DV.docViewSpec -- TODO pass-in trash mode
    ]

ngramsViewSpec :: {mode :: NV.Mode} -> Spec State Props Action
ngramsViewSpec {mode} =
  cmapProps (\{loaded, path, dispatch} -> {mode,loaded,path, dispatch})
            (noState NV.ngramsTableSpec)
