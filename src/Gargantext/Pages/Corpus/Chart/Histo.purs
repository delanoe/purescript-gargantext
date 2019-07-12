module Gargantext.Pages.Corpus.Chart.Histo where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (foldl)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Gargantext.Config -- (End(..), Path(..), TabType, toUrl)
import Gargantext.Config.REST (get)
import React (ReactClass, ReactElement, createElement)
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (Spec)

import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Type
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Color
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data
import Gargantext.Utils.Reactix as R2
import Gargantext.Pages.Corpus.Chart.Utils as U

type Path =
  { corpusId :: Int
  , tabType  :: TabType
  }

newtype ChartMetrics = ChartMetrics
  { "data" :: HistoMetrics
  }

instance decodeChartMetrics :: DecodeJson ChartMetrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .? "data"
    pure $ ChartMetrics { "data": d }

newtype HistoMetrics = HistoMetrics
  { dates :: Array String
  , count :: Array Number
  }

instance decodeHistoMetrics :: DecodeJson HistoMetrics where
  decodeJson json = do
    obj   <- decodeJson json
    d <- obj .? "dates"
    c <- obj .? "count"
    pure $ HistoMetrics { dates : d , count: c}

type Loaded = HistoMetrics

chartOptions :: HistoMetrics -> Options
chartOptions (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Histogram"
  , subTitle  : "Distribution of publications over time"
  , xAxis     : xAxis' dates'
  , yAxis     : yAxis' { position: "left", show: true, min:0}
  , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {value: n, itemStyle : itemStyle {color:grey}}) count']
  , addZoom   : true
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  }


getMetrics :: Path -> Aff HistoMetrics
getMetrics {corpusId, tabType} = do
  ChartMetrics ms <- get $ toUrl Back (Chart {chartType: Histo, tabType: tabType}) $ Just corpusId
  pure ms."data"

histoSpec :: Spec {} Path Void
histoSpec = R2.elSpec $ R.hooksComponent "LoadedMetricsHisto" cpt
  where
    cpt p _ = do
      setReload <- R.useState' 0

      pure $ metricsLoadView setReload p

metricsLoadView :: R.State Int -> Path -> R.Element
metricsLoadView setReload p = R.createElement el p []
  where
    el = R.hooksComponent "MetricsLoadedHistoView" cpt
    cpt p _ = do
      useLoader p getMetrics $ \{loaded} ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> HistoMetrics -> R.Element
loadedMetricsView setReload loaded = U.reloadButtonWrap setReload $ R2.buff $ chart $ chartOptions loaded
