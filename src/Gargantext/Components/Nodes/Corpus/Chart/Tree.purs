module Gargantext.Components.Nodes.Corpus.Chart.Tree where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (TreeNode, Trees(..), mkTree)
import Gargantext.Components.Charts.Options.Font (mkTooltip, templateFormatter)
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsLoadView, metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType)

newtype Metrics = Metrics {
    "data" :: Array TreeNode
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ Metrics { "data": d }
instance encodeMetrics :: EncodeJson Metrics where
  encodeJson (Metrics { "data": d }) =
       "data" := encodeJson d
    ~> jsonEmptyObject

type Loaded  = Array TreeNode

scatterOptions :: Array TreeNode -> Options
scatterOptions nodes = Options
  { mainTitle : "Tree"
  , subTitle  : "Tree Sub Title"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position : "", show: false, min:0}
  , series    : [ mkTree TreeMap nodes]
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
-- TODO improve the formatter:
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-obama

  }

getMetrics :: Session -> Tuple Reload (Record Path) -> Aff (HashedResponse Loaded)
getMetrics session (_ /\ { corpusId, limit, listId, tabType }) = do
  HashedResponse { md5, value: Metrics ms } <- get session chart
  pure $ HashedResponse { md5, value: ms."data" }
  where
    chart = Chart {chartType : ChartTree, limit, listId, tabType} (Just corpusId)

getMetricsMD5 :: Session -> Tuple Reload (Record Path) -> Aff String
getMetricsMD5 session (_ /\ { corpusId, limit, listId, tabType }) = do
  get session $ ChartMD5 { chartType: ChartTree, listId, tabType } (Just corpusId)

tree :: Record Props -> R.Element
tree props = R.createElement treeCpt props []

treeCpt :: R.Component Props
treeCpt = R.hooksComponent "G.C.N.C.C.T.tree" cpt
  where
    cpt {path, session} _ = do
      reload <- R.useState' 0
      --pure $ metricsLoadView {getMetrics, loaded, path, reload, session}
      pure $ metricsWithCacheLoadView { getMetrics, getMetricsMD5, keyFunc: const "tree", loaded, path, reload, session }

loaded :: Session -> Record Path -> R.State Reload -> Loaded -> R.Element
loaded session path reload loaded =
  H.div {} [
    U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartTree, path, reload, session }
  , chart (scatterOptions loaded)
  ]
