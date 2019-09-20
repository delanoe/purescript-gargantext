module Gargantext.Pages.Corpus.Chart.Histo where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Config
import Gargantext.Config.REST (get)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Color
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Utils.Reactix as R2
import Gargantext.Pages.Corpus.Chart.Utils as U

type Path = { corpusId :: Int, tabType  :: TabType }

type Props = ( path :: Path, ends :: Ends )

newtype ChartMetrics = ChartMetrics { "data" :: HistoMetrics }

instance decodeChartMetrics :: DecodeJson ChartMetrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ ChartMetrics { "data": d }

newtype HistoMetrics = HistoMetrics { dates :: Array String, count :: Array Number }

instance decodeHistoMetrics :: DecodeJson HistoMetrics where
  decodeJson json = do
    obj   <- decodeJson json
    d <- obj .: "dates"
    c <- obj .: "count"
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

getMetrics :: Ends -> Path -> Aff HistoMetrics
getMetrics ends {corpusId, tabType} = do
  ChartMetrics ms <- get $ url ends chart
  pure ms."data"
  where chart = Chart {chartType: Histo, tabType: tabType} (Just corpusId)

histo :: Record Props -> R.Element
histo props = R.createElement histoCpt props []

histoCpt :: R.Component Props
histoCpt = R.hooksComponent "LoadedMetricsHisto" cpt
  where
    cpt {ends,path} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadView ends setReload path

metricsLoadView :: Ends -> R.State Int -> Path -> R.Element
metricsLoadView ends setReload path = R.createElement el {ends,path} []
  where
    el = R.hooksComponent "MetricsLoadedHistoView" cpt
    cpt {path,ends} _ = do
      useLoader path (getMetrics ends) $ \loaded ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> HistoMetrics -> R.Element
loadedMetricsView setReload loaded = U.reloadButtonWrap setReload $ chart $ chartOptions loaded
