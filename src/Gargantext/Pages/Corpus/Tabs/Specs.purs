module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))

import Gargantext.Config (TabType(..), TabSubType(..))
import Gargantext.Pages.Corpus.Tabs.Types (Props)

import Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable as NV
import Gargantext.Pages.Corpus.Dashboard (globalPublis)
import Gargantext.Components.Charts.Options.ECharts (chart) as ECharts
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Tab as Tab
import Thermite (Spec, hideState, noState, cmapProps)

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"  $ docs
    , Tuple "Authors"    $ ngramsViewSpec {mode: NV.Authors}
    , Tuple "Sources"    $ ngramsViewSpec {mode: NV.Sources}
    , Tuple "Institutes" $ ngramsViewSpec {mode: NV.Institutes}
    , Tuple "Terms"      $ ngramsViewSpec {mode: NV.Terms}
    , Tuple "Trash"      $ docs -- TODO pass-in trash mode
    ]
  where
    -- TODO totalRecords
    chart = ECharts.chart globalPublis
    docs = cmapProps (\{path: nodeId} ->
                       {nodeId, chart, tabType: TabCorpus TabDocs, totalRecords: 4736}) $
           noState DT.docViewSpec

ngramsViewSpec :: {mode :: NV.Mode} -> Spec Tab.State Props Tab.Action
ngramsViewSpec {mode} =
  cmapProps (\{loaded, path, dispatch} -> {mode,loaded,path, dispatch})
            (noState NV.ngramsTableSpec)
