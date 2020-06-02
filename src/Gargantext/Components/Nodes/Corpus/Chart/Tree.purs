module Gargantext.Components.Nodes.Corpus.Chart.Tree where

import Prelude (bind, pure, ($))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (TreeNode, Trees(..), mkTree)
import Gargantext.Components.Charts.Options.Font (mkTooltip, templateFormatter)
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Components.Nodes.Corpus.Chart.Types (ListPath, Props)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType)

newtype Metrics = Metrics
  { "data" :: Array TreeNode
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ Metrics { "data": d }

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

getMetrics :: Session -> Record ListPath -> Aff Loaded
getMetrics session {corpusId, listId, limit, tabType} = do
  Metrics ms <- get session chart
  pure ms."data"
  where
    chart = Chart {chartType : ChartTree, tabType: tabType} (Just corpusId)

tree :: Record (Props ListPath) -> R.Element
tree props = R.createElement treeCpt props []

treeCpt :: R.Component (Props ListPath)
treeCpt = R.hooksComponent "LoadedMetrics" cpt
  where
    cpt {path, session} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadView session setReload path

metricsLoadView :: Session -> R.State Int -> Record ListPath -> R.Element
metricsLoadView session setReload path = R.createElement el path []
  where
    el = R.hooksComponent "MetricsLoadView" cpt
    cpt p _ = do
      useLoader p (getMetrics session) $ \loaded ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> Loaded -> R.Element
loadedMetricsView setReload loaded =
  H.div {}
  [ U.reloadButton setReload
  , chart (scatterOptions loaded) ]
