module Gargantext.Components.Nodes.Lists.Tabs where

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
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

type Props = (
    asyncTasks :: GAT.ReductorAction
  , cacheState :: R.State NTypes.CacheState
  , corpusData :: CorpusData
  , corpusId   :: Int
  , session    :: Session
  )

type PropsWithKey = (
  key        :: String
  | Props
  )

tabs :: Record PropsWithKey -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component PropsWithKey
tabsCpt = R.hooksComponentWithModule thisModule "tabs" cpt
  where
    cpt { asyncTasks, cacheState, corpusData: corpusData@{ defaultListId }, corpusId, session } _ = do
      (selected /\ setSelected) <- R.useState' 0

      pure $ Tab.tabs { selected, tabs: tabs' }
      where
        tabs' = [ "Authors"    /\ view Authors
                , "Institutes" /\ view Institutes
                , "Sources"    /\ view Sources
                , "Terms"      /\ view Terms ]
        view mode = ngramsView { asyncTasks, cacheState, corpusData, corpusId, mode, session }

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: Record NgramsViewProps -> R.Element
ngramsView props = R.createElement ngramsViewCpt props []

ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = R.hooksComponentWithModule thisModule "ngramsView" cpt
  where
    cpt { asyncTasks
        , cacheState
        , corpusData: { defaultListId }
        , corpusId
        , mode
        , session } _ = do

      chartType <- R.useState' Histo
      chartsReload <- R.useState' 0

      pure $ R.fragment
        ( charts tabNgramType chartType chartsReload
        <> [ NT.mainNgramsTable { afterSync: afterSync chartsReload
                                , asyncTasks
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
        afterSync (_ /\ setChartsReload) _ = do
          case mNgramsType of
            Just ngramsType -> do
              -- NOTE: No need to recompute chart, after ngrams are sync this
              -- should be recomputed already
              -- We just refresh it
              -- _ <- recomputeChart session chartType ngramsType corpusId listId
              liftEffect $ setChartsReload $ (+) 1
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
                                             $ R.unsafeEventValue e
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

        chart Authors    = pie     { path, session }
        chart Institutes = tree    { path, session }
        chart Sources    = bar     { path, session }
        chart Terms      = metrics { path, session }
