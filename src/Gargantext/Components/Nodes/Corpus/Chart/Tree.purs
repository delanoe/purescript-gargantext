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
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Corpus.Chart.Tree"

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

getMetricsHash :: Session -> Tuple Reload (Record Path) -> Aff String
getMetricsHash session (_ /\ { corpusId, limit, listId, tabType }) = do
  get session $ ChartHash { chartType: ChartTree, listId: mListId, tabType } (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: ChartTree, limit, listId: mListId, tabType} (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

handleResponse :: HashedResponse Metrics -> Loaded
handleResponse (HashedResponse { value: Metrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path@{ corpusId, limit, listId, tabType }) = GUC.makeGetRequest session $ chartUrl path

tree :: Record Props -> R.Element
tree props = R.createElement treeCpt props []

treeCpt :: R.Component Props
treeCpt = R.hooksComponentWithModule thisModule "tree" cpt
  where
    cpt {path, session} _ = do
      reload <- R.useState' 0
      pure $ metricsWithCacheLoadView {
          getMetricsHash
        , handleResponse
        , loaded
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        }

loaded :: Record MetricsProps -> Loaded -> R.Element
loaded { path, reload, session } loaded =
  H.div {} [
  {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartTree, path, reload, session }
  , -} chart (scatterOptions loaded)
  ]
