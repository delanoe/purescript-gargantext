module Gargantext.Pages.Texts.Tabs.Specs where

import Prelude hiding (div)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Gargantext.Config (CTabNgramType(..), End(..), Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Pages.Texts.Tabs.Types (Props, PropsRow)

import Gargantext.Pages.Corpus.Chart.Histo (histoSpec)
import Gargantext.Pages.Corpus.Chart.Metrics (metricsSpec)
import Gargantext.Pages.Corpus.Chart.Pie  (pieSpec, barSpec)
import Gargantext.Pages.Corpus.Chart.Tree (treeSpec)

import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Charts.Options.ECharts (chart) as ECharts
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Tab as Tab
import Gargantext.Utils.Reactix as R2

import Reactix.DOM.HTML as H
import React (ReactElement, ReactClass, createElement, Children)
import Thermite (Spec, hideState, noState, cmapProps, createClass)

data Mode = MoreLikeFav | MoreLikeTrash

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType MoreLikeFav    = CTabAuthors  -- TODO
modeTabType MoreLikeTrash  = CTabSources  -- TODO

elt :: Props -> ReactElement
elt props = createElement tabsClass props []

tabsClass :: ReactClass { children :: Children | PropsRow }
tabsClass = createClass "CorpusTabs" pureTabs $ const {}

pureTabs :: Spec {} Props Void
pureTabs = hideState (const {activeTab: 0}) statefulTabs

statefulTabs :: Spec Tab.State Props Tab.Action
statefulTabs =
  Tab.tabs identity identity $ fromFoldable
    [ Tuple "Documents"  $ docs
    , Tuple "Trash"      $ trash
    , Tuple "More like fav" $ moreLikeFav
    , Tuple "More like trash" $ moreLikeTrash
    ]
  where
    -- TODO totalRecords

    docs = noState ( cmapProps (\{corpusId} -> {corpusId, tabType: TabCorpus TabDocs}) histoSpec
                              <>
                     (cmapProps (\{corpusId, corpusData: {defaultListId}} ->
                                  { nodeId: corpusId
                                  -- ^ TODO merge nodeId and corpusId in DT
                                  , chart  : H.div {} []
                                  , tabType: TabCorpus TabDocs
                                  , totalRecords: 4737
                                  , listId: defaultListId
                                  , corpusId: Just corpusId}) $ noState DT.docViewSpec
                                  )
                    )

    moreLikeFav = noState (cmapProps (\{corpusId, corpusData: {defaultListId}} ->
                                  { nodeId: corpusId
                                    -- ^ TODO merge nodeId and corpusId in DT
                                  , chart  : H.div {} []
                                  , tabType: TabCorpus TabMoreLikeFav
                                  , totalRecords: 4737
                                  , listId: defaultListId
                                  , corpusId: Just corpusId}) $ noState DT.docViewSpec
                    )

    moreLikeTrash = noState (cmapProps (\{corpusId, corpusData: {defaultListId}} ->
                                        { nodeId: corpusId
                                          -- ^ TODO merge nodeId and corpusId in DT
                                        , chart  : H.div {} []
                                        , tabType: TabCorpus TabMoreLikeTrash
                                        , totalRecords: 4737
                                        , listId: defaultListId
                                        , corpusId: Just corpusId}) $ noState DT.docViewSpec
                          )

    trash = cmapProps (\{corpusId, corpusData: {defaultListId}} ->
                        { nodeId: corpusId
                        , chart  : H.div {} []
                        , tabType: TabCorpus TabTrash
                        , totalRecords: 4736
                        , listId: defaultListId
                        , corpusId: Nothing}) $ noState DT.docViewSpec
