module Gargantext.Pages.Corpus.Chart.Histo where

import Data.Array (foldl)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Gargantext.Config -- (End(..), Path(..), TabType, toUrl)
import Gargantext.Config.REST (get)
import React (ReactClass, ReactElement, createElement)
import Thermite (Spec, Render, defaultPerformAction, simpleSpec, createClass)
import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Type
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Color
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data

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

loadedMetricsSpec :: Spec {} (Loader.InnerProps Path Loaded ()) Void
loadedMetricsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} (Loader.InnerProps Path Loaded ()) Void
    render dispatch {loaded:histoMetrics} {} _ = [chart (chartOptions histoMetrics)]

chartOptions :: HistoMetrics -> Options
chartOptions (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Histogram"
  , subTitle  : "Distribution of publications over time"
  , xAxis     : xAxis' dates'
  , yAxis     : yAxis' { position: "left", show: true, min:0}
  , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", value: n, itemStyle : itemStyle {color:grey}}) count']
  , addZoom   : true
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  }

metricsLoader :: Loader.Props' Path HistoMetrics -> ReactElement
metricsLoader props = createElement metricsLoaderClass props []
  where
    metricsLoaderClass :: ReactClass (Loader.Props Path HistoMetrics)
    metricsLoaderClass = Loader.createLoaderClass "MetricsLoader" getMetrics

    getMetrics :: Path -> Aff HistoMetrics
    getMetrics {corpusId, tabType} = do
      ChartMetrics ms <- get $ toUrl Back (Chart {chartType: Histo, tabType: tabType}) $ Just corpusId
      pure ms."data"

histoSpec :: Spec {} Path Void
histoSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Path Void
    render dispatch path {} _ =
      [ metricsLoader
        { path
        , component: createClass "LoadedMetrics" loadedMetricsSpec (const {})
        } ]
