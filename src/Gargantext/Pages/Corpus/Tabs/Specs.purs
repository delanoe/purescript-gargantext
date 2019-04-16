module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Gargantext.Config (TabType(..), TabSubType(..))
import Gargantext.Config (CTabNgramType(..), End(..), Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Pages.Corpus.Tabs.Types (Props)

import Gargantext.Pages.Corpus.Chart.Histo (histoSpec)
import Gargantext.Pages.Corpus.Chart.Metrics (metricsSpec)
import Gargantext.Pages.Corpus.Chart.Pie  (pieSpec)
import Gargantext.Pages.Corpus.Chart.Tree (treeSpec)

import Gargantext.Pages.Corpus.Dashboard (globalPublis)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Charts.Options.ECharts (chart) as ECharts
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Tab as Tab

import React (ReactElement)
import React.DOM (div)
import Thermite (Spec, hideState, noState, cmapProps)

data Mode = Authors | Sources | Institutes | Terms

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType Authors    = CTabAuthors
modeTabType Sources    = CTabSources
modeTabType Institutes = CTabInstitutes
modeTabType Terms      = CTabTerms

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"  $ docs
    , Tuple "Authors"    $ ngramsViewSpec {mode: Authors   }
    , Tuple "Sources"    $ ngramsViewSpec {mode: Sources   }
    , Tuple "Institutes" $ ngramsViewSpec {mode: Institutes}
    , Tuple "Terms"      $ ngramsViewSpec {mode: Terms     }
    , Tuple "Trash"      $ trash
    ]
  where
    -- TODO totalRecords

    docs = noState ( cmapProps (\{path: corpusId} -> {corpusId : corpusId}) histoSpec
                              <>
                     (cmapProps (\{path: nodeId} -> { nodeId : nodeId
                                         , chart  : div [][] -- ECharts.chart globalPublis
                                         , tabType: TabCorpus TabDocs
                                         , totalRecords: 4736}) $ noState DT.docViewSpec
                                    )
                    )
    
    trash = cmapProps (\{path: nodeId} -> { nodeId
                                          , chart: div [][]
                                          , tabType: TabCorpus TabTrash
                                          , totalRecords: 4736}) $ noState DT.docViewSpec


ngramsViewSpec :: {mode :: Mode} -> Spec Tab.State Props Tab.Action
ngramsViewSpec {mode} =
  noState (
    cmapProps (\{loaded: {defaultListId}, path: corpusId} ->
                          {corpusId, listId: defaultListId, tabType, limit: (Just 1000)}) -- TODO limit should be select in the chart by default it is 1000
              (chart mode) <>
    cmapProps (\{loaded: {defaultListId}, path, dispatch} ->
                {loaded: {defaultListId}, path, dispatch, tabType})
              NT.mainNgramsTableSpec
  )
  where
    tabType = TabCorpus $ TabNgramType $ modeTabType mode
    chart Authors = pieSpec
    chart Sources = pieSpec
    chart Institutes = treeSpec
    chart Terms      = metricsSpec
