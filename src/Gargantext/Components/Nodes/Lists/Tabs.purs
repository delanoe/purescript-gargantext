module Gargantext.Components.Nodes.Lists.Tabs where

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie  (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart (getChartFunction)
import Gargantext.Components.Nodes.Corpus.Chart.Utils (mNgramsTypeFromTabType)
import Gargantext.Components.Nodes.Lists.Types
import Gargantext.Components.Search as S
import Gargantext.Components.Tab as Tab
import Gargantext.Sessions (Session)
import Gargantext.Types (ChartType(..), CTabNgramType(..), Mode(..), TabSubType(..), TabType(..), chartTypeFromString, modeTabType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Lists.Tabs"

type Props = (
    appReload         :: GUR.ReloadS
  , asyncTasksRef     :: R.Ref (Maybe GAT.Reductor)
  , cacheState        :: R.State CacheState
  , corpusData        :: CorpusData
  , corpusId          :: Int
  , session           :: Session
  , sidePanelTriggers :: Record SidePanelTriggers
  , treeReloadRef     :: GUR.ReloadWithInitializeRef
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
    cpt { appReload
        , asyncTasksRef
        , cacheState
        , corpusData
        , corpusId
        , session
        , sidePanelTriggers
        , treeReloadRef } _ = do
      (selected /\ setSelected) <- R.useState' 0

      pure $ Tab.tabs { selected, tabs: tabs' }
      where
        tabs' = [ "Terms"      /\ view Terms
                , "Authors"    /\ view Authors
                , "Institutes" /\ view Institutes
                , "Sources"    /\ view Sources
                ]
        view mode = ngramsView { appReload
                               , asyncTasksRef
                               , cacheState
                               , corpusData
                               , corpusId
                               , mode
                               , session
                               , sidePanelTriggers
                               , treeReloadRef } []

type NgramsViewProps = ( mode :: Mode | Props )

ngramsView :: R2.Component NgramsViewProps
ngramsView = R.createElement ngramsViewCpt

ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = R.hooksComponentWithModule thisModule "ngramsView" cpt
  where
    cpt { appReload
        , asyncTasksRef
        , cacheState
        , corpusData: { defaultListId }
        , corpusId
        , mode
        , session
        , sidePanelTriggers
        , treeReloadRef
        } _ = do

      chartType <- R.useState' Histo
      chartsReload <- GUR.new
      let path = NTC.initialPageParams session corpusId [listId] tabType
      let chartParams = {
          corpusId
        , limit: Just path.params.limit
        , listId
        , tabType
        }

      pure $ R.fragment
        ( charts chartParams tabNgramType chartType chartsReload
        <> [ NT.mainNgramsTable { afterSync: afterSync chartsReload
                                , appReload
                                , asyncTasksRef
                                , cacheState
                                , defaultListId
                                , path
                                , sidePanelTriggers
                                , tabNgramType
                                , treeReloadRef
                                , withAutoUpdate: false
                                } []
           ]
        )
      where
        afterSync chartsReload _ = do
          case mNgramsType of
            Just ngramsType -> do
              -- NOTE: No need to recompute chart, after ngrams are sync this
              -- should be recomputed already
              -- We just refresh it
              -- _ <- recomputeChart session chartType ngramsType corpusId listId
              liftEffect $ GUR.bump chartsReload
            Nothing         -> pure unit

        tabNgramType = modeTabType mode
        tabType      = TabCorpus (TabNgramType tabNgramType)
        mNgramsType  = mNgramsTypeFromTabType tabType
        listId       = defaultListId
        initialPath  = { corpusId
                       -- , limit: Just 1000
                       , listId
                       , tabType
                       }

        charts params CTabTerms (chartType /\ setChartType) _ = [
          H.div {className: "row"}
                [ H.div {className: "col-12 d-flex justify-content-center"}
                  [ H.img { src: "images/Gargantextuel-212x300.jpg"
                          , id: "funnyimg"
                        }
                  ]
                ]

          {-
              R2.select { className: "form-control"
                        , defaultValue: show chartType
                        , on: { change: \e -> setChartType
                                             $ const
                                             $ fromMaybe Histo
                                             $ chartTypeFromString
                                             $ R.unsafeEventValue e
                              }
                        } [
                H.option { value: show Histo     } [ H.text $ show Histo     ]
              , H.option { value: show Scatter   } [ H.text $ show Scatter   ]
              , H.option { value: show ChartBar  } [ H.text $ show ChartBar  ]
              , H.option { value: show ChartPie  } [ H.text $ show ChartPie  ]
              , H.option { value: show ChartTree } [ H.text $ show ChartTree ]
              ]
            ]
          ]
        , getChartFunction chartType $ { path: params, session }
        -}
        ]
        charts params _ _ _         = [ chart params mode ]

        chart path Authors    = pie     { path, session }
        chart path Institutes = tree    { path, session }
        chart path Sources    = bar     { path, session }
        chart path Terms      = metrics { path, session }
