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

type Props = ( session    :: Session
             , corpusId   :: Int
             , corpusData :: CorpusData 
             )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R.hooksComponent "G.C.N.L.T.tabs" cpt
  where
    cpt { corpusData: corpusData@{ defaultListId }, corpusId, session } _ = do
      (selected /\ setSelected) <- R.useState' 0
      pure $ Tab.tabs { selected, tabs: tabs' }
      where
        tabs' = [ "Authors"    /\ view Authors
                , "Institutes" /\ view Institutes
                , "Sources"    /\ view Sources
                , "Terms"      /\ view Terms ]
        view mode = ngramsView { corpusData, corpusId, mode, session }

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: Record NgramsViewProps -> R.Element
ngramsView props = R.createElement ngramsViewCpt props []

ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = R.hooksComponent "G.C.N.L.T.ngramsView" cpt
  where
    cpt { corpusData: { defaultListId }
        , corpusId
        , mode
        , session } _ = do
      chartType <- R.useState' Histo

      pure $ R.fragment
        ( charts tabNgramType chartType
        <> [ NT.mainNgramsTable { defaultListId
                                , nodeId: corpusId
                                , session
                                , tabNgramType
                                , tabType
                                , withAutoUpdate: false
                                }
           ]
        )
      where
        tabNgramType = modeTabType mode
        tabType      = TabCorpus (TabNgramType tabNgramType)
        listId       = defaultListId
        path         = { corpusId
                       , limit: Just 1000
                       , listId
                       , tabType
                       }

        charts CTabTerms (chartType /\ setChartType) = [
          H.div { className: "row chart-type-selector" } [
            H.div { className: "col-md-3" } [
              R2.select { className: "form-control"
                        ,  on: { change: \e -> setChartType
                                             $ const
                                             $ fromMaybe Histo
                                             $ chartTypeFromString
                                             $ R2.unsafeEventValue e
                               }
                        , defaultValue: show chartType } [
                H.option { value: show Histo     } [ H.text $ show Histo     ]
              , H.option { value: show Scatter   } [ H.text $ show Scatter   ]
              , H.option { value: show ChartBar  } [ H.text $ show ChartBar  ]
              , H.option { value: show ChartPie  } [ H.text $ show ChartPie  ]
              , H.option { value: show ChartTree } [ H.text $ show ChartTree ]
              ]
            ]
          ]
        , getChartFunction chartType $ { session, path }
        ]
        charts _ _       = [ chart mode ]
        chart Authors    = pie     { session, path }
        chart Sources    = bar     { session, path }
        chart Institutes = tree    { session, path }
        chart Terms      = metrics { session, path }
