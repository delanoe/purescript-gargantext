module Gargantext.Pages.Corpus.Tabs.Specs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Gargantext.Config (CTabNgramType(..), End(..), Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Pages.Corpus.Tabs.Types (Props, PropsRow)

import Gargantext.Pages.Corpus.Chart.Histo (histoSpec)
import Gargantext.Pages.Corpus.Chart.Metrics (metricsSpec)
import Gargantext.Pages.Corpus.Chart.Pie  (pieSpec, barSpec)
import Gargantext.Pages.Corpus.Chart.Tree (treeSpec)

import Gargantext.Pages.Corpus.Dashboard (globalPublis)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Charts.Options.ECharts (chart) as ECharts
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Tab as Tab
import Gargantext.Utils.Reactix as R2

import React (ReactElement, ReactClass, createElement, Children)
import React.DOM (div)
import Thermite (Spec, hideState, noState, cmapProps, createClass)

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

elt :: Props -> ReactElement
elt props = createElement tabsClass props []

tabsClass :: ReactClass { children :: Children | PropsRow }
tabsClass = createClass "CorpusTabs" pureTabs (const {})

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"  $ docs
    , Tuple "Sources"    $ ngramsViewSpec {mode: Sources   }
    , Tuple "Authors"    $ ngramsViewSpec {mode: Authors   }
    , Tuple "Institutes" $ ngramsViewSpec {mode: Institutes}
    , Tuple "Terms"      $ ngramsViewSpec {mode: Terms     }
    , Tuple ""           $ trash -- TODO empty here
    , Tuple "Trash"      $ trash
    ]
  where
    -- TODO totalRecords

    docs = noState ( cmapProps (\{corpusId} -> {corpusId, tabType: TabCorpus TabDocs}) histoSpec
                              <>
                     (cmapProps (\{corpusId, corpusData: {defaultListId}} ->
                                  { nodeId: corpusId
                                  -- ^ TODO merge nodeId and corpusId in DT
                                  , chart  : div [][]
                                  , tabType: TabCorpus TabDocs
                                  , totalRecords: 4737
                                  , listId: defaultListId
                                  , corpusId: Just corpusId}) $ noState DT.docViewSpec
                                  )
                    )
    trash = cmapProps (\{corpusId, corpusData: {defaultListId}} ->
                        { nodeId: corpusId
                        , chart: div [][]
                        , tabType: TabCorpus TabTrash
                        , totalRecords: 4736
                        , listId: defaultListId
                        , corpusId: Nothing}) $ noState DT.docViewSpec


ngramsViewSpec :: {mode :: Mode} -> Spec Tab.State Props Tab.Action
ngramsViewSpec {mode} =
  noState $ chart mode <>
    cmapProps (\{corpusData: {defaultListId}, corpusId: nodeId} ->
                {defaultListId, nodeId, tabType})
              (NT.mainNgramsTableSpec (modeTabType mode))

  where
    tabType = TabCorpus $ TabNgramType $ modeTabType mode
    chart Authors    = cmapProps (\{corpusId} -> {corpusId, tabType}) pieSpec
    chart Sources    = cmapProps (\{corpusId} -> {corpusId, tabType}) barSpec

    chart Institutes = cmapProps (\{corpusData: {defaultListId}, corpusId} ->
                          {corpusId, listId: defaultListId, tabType, limit: (Just 1000)})
                      treeSpec

    chart Terms      = cmapProps (\{corpusData: {defaultListId}, corpusId} ->
                          {corpusId, listId: defaultListId, tabType, limit: (Just 1000)})
                          -- TODO limit should be select in the chart by default it is 1000
                      metricsSpec
