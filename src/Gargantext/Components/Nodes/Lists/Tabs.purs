module Gargantext.Components.Nodes.Lists.Tabs where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Sessions (Session)
import Gargantext.Types (Mode(..), TabSubType(..), TabType(..), modeTabType)

type Props =
  ( session :: Session
  , corpusId :: Int
  , corpusData :: CorpusData )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "CorpusTabs" cpt
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
ngramsViewCpt = R.staticComponent "ListsNgramsView" cpt
  where
    cpt {mode, session, corpusId, corpusData: {defaultListId}} _ =
      R.fragment
        [ chart mode
        , NT.mainNgramsTable
            {session, defaultListId, nodeId: corpusId, tabType, tabNgramType, withAutoUpdate: false}
        ]
      where
        tabNgramType = modeTabType mode
        tabType = TabCorpus (TabNgramType tabNgramType)
        listId = 0 -- TODO!
        path = {corpusId, tabType}
        path2 = {corpusId, listId, tabType, limit: (Just 1000)} -- todo
        chart Authors = pie {session, path}
        chart Sources = bar {session, path}
        chart Institutes = tree {session, path: path2}
        chart Terms      = metrics {session, path: path2}
