module Gargantext.Components.Nodes.Lists.Tabs where

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart (getChartFunction)
import Gargantext.Sessions (Session)
import Gargantext.Types (ChartType(..), CTabNgramType(..), Mode(..), TabSubType(..), TabType(..), chartTypeFromString, modeTabType)
import Gargantext.Utils.Reactix as R2

type Props =
  ( session :: Session
  , corpusId :: Int
  , corpusData :: CorpusData )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "G.C.N.L.T.tabs" cpt
  where
    cpt {session, corpusId, corpusData: corpusData@{defaultListId}} _ = do
      (selected /\ setSelected) <- R.useState' 0
      pure $ Tab.tabs { tabs: tabs', selected }
      where
        tabs' = [ "Sources"    /\ view Sources
                , "Authors"    /\ view Authors
                , "Institutes" /\ view Institutes
                , "Terms"      /\ view Terms ]
        view mode = ngramsView {mode, session, corpusId, corpusData}

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: Record NgramsViewProps -> R.Element
ngramsView props = R.createElement ngramsViewCpt props []

ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = R.hooksComponent "G.C.N.L.T.ngramsView" cpt
  where
    cpt {mode, session, corpusId, corpusData: {defaultListId}} _ = do
      chartType <- R.useState' Scatter

      pure $ R.fragment
        ( charts tabNgramType chartType
        <> [
          NT.mainNgramsTable
            {session, defaultListId, nodeId: corpusId, tabType, tabNgramType, withAutoUpdate: false}
        ]
        )
      where
        tabNgramType = modeTabType mode
        tabType = TabCorpus (TabNgramType tabNgramType)
        listId = 0 -- TODO!
        path = {corpusId, listId, tabType, limit: (Just 1000)}

        charts CTabTerms (chartType /\ setChartType) = [
          getChartFunction chartType $ { session, path }
        , R2.select { on: { change: \e -> setChartType $ const $ fromMaybe Scatter $ chartTypeFromString $ R2.unsafeEventValue e }
                    , defaultValue: show chartType } [
            H.option { value: show Scatter } [ H.text $ show Scatter ]
          , H.option { value: show ChartTree } [ H.text $ show ChartTree ]
          ]
        ]
        charts _ _ = [ chart mode ]

        chart Authors = pie { session, path }
        chart Sources = bar { session, path }
        chart Institutes = tree { session, path }
        chart Terms      = metrics { session, path }
