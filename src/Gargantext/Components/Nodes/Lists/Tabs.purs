module Gargantext.Components.Nodes.Lists.Tabs where

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Nodes.Corpus.Chart.API (recomputeChart)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart (getChartFunction)
import Gargantext.Components.Nodes.Corpus.Chart.Utils (mNgramsTypeFromTabType)
import Gargantext.Components.Nodes.Lists.Types as NTypes
import Gargantext.Components.Tab as Tab
import Gargantext.Sessions (Session)
import Gargantext.Types (ChartType(..), CTabNgramType(..), Mode(..), TabSubType(..), TabType(..), chartTypeFromString, modeTabType)
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Lists.Tabs"

type Props = ( cacheState :: R.State NTypes.CacheState
             , corpusData :: CorpusData
             , corpusId   :: Int
             , session    :: Session
             )

tabs :: Record Props -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component Props
tabsCpt = R2.hooksComponent thisModule "tabs" cpt
  where
    cpt { cacheState, corpusData: corpusData@{ defaultListId }, corpusId, session } _ = do
      (selected /\ setSelected) <- R.useState' 0

      pure $ Tab.tabs { selected, tabs: tabs' }
      where
        tabs' = [ "Authors"    /\ view Authors
                , "Institutes" /\ view Institutes
                , "Sources"    /\ view Sources
                , "Terms"      /\ view Terms ]
        view mode = ngramsView { cacheState, corpusData, corpusId, mode, session }

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: Record NgramsViewProps -> R.Element
ngramsView props = R.createElement ngramsViewCpt props []

ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = R2.hooksComponent thisModule "ngramsView" cpt
  where
    cpt { cacheState
        , corpusData: { defaultListId }
        , corpusId
        , mode
        , session } _ = do
      chartType <- R.useState' Histo
      chartsReload <- R.useState' 0

      pure $ R.fragment
        ( charts tabNgramType chartType chartsReload
        <> [ NT.mainNgramsTable { afterSync: afterSync (fst chartType) chartsReload
                                , cacheState
                                , defaultListId
                                , nodeId: corpusId
                                , session
                                , tabNgramType
                                , tabType
                                , withAutoUpdate: false
                                }
           ]
        )
      where
        afterSync chartType (_ /\ setChartsReload) _ = do
          case mNgramsType of
            Just ngramsType -> do
              launchAff_ $ do
                recomputeChart session chartType ngramsType corpusId listId
              setChartsReload $ (+) 1
            Nothing         -> pure unit

        tabNgramType = modeTabType mode
        tabType      = TabCorpus (TabNgramType tabNgramType)
        mNgramsType = mNgramsTypeFromTabType tabType
        listId       = defaultListId
        path         = { corpusId
                       , limit: Just 1000
                       , listId
                       , tabType
                       }

        charts CTabTerms (chartType /\ setChartType) _ = [
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
        charts _ _ _       = [ chart mode ]

        chart Authors    = pie     { session, path }
        chart Institutes = tree    { session, path }
        chart Sources    = bar     { session, path }
        chart Terms      = metrics { session, path }
