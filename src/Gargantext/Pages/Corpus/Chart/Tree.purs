module Gargantext.Pages.Corpus.Chart.Tree where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Config
import Gargantext.Config.REST (get)
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (Spec)

import Gargantext.Prelude
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Font
import Gargantext.Utils.Reactix as R2
import Gargantext.Pages.Corpus.Chart.Utils as U


type Path =
  { corpusId :: Int
  , listId   :: Int
  , tabType  :: TabType
  , limit    :: Maybe Int
  }


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

getMetrics :: Path -> Aff Loaded
getMetrics {corpusId, listId, limit, tabType} = do
  Metrics ms <- get $ toUrl endConfigStateful Back (Chart {chartType : ChartTree, tabType: tabType}) $ Just corpusId
  pure ms."data"

treeSpec :: Spec {} Path Void
treeSpec = R2.elSpec $ R.hooksComponent "LoadedMetrics" cpt
  where
    cpt p _ = do
      setReload <- R.useState' 0

      pure $ metricsLoadView setReload p

metricsLoadView :: R.State Int -> Path -> R.Element
metricsLoadView setReload p = R.createElement el p []
  where
    el = R.hooksComponent "MetricsLoadView" cpt
    cpt p _ = do
      useLoader p getMetrics $ \{loaded} ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> Loaded -> R.Element
loadedMetricsView setReload loaded = H.div {} [
    U.reloadButton setReload
  , R2.buff $ chart (scatterOptions loaded)
  ]
