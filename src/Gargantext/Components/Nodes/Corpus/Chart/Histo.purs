module Gargantext.Components.Nodes.Corpus.Chart.Histo where

import Prelude (bind, map, pure, ($))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (seriesBarD1)
import Gargantext.Components.Charts.Options.Color (grey)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType)

type Path = { corpusId :: Int, tabType  :: TabType }

type Props = ( path :: Path, session :: Session )

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
  , addZoom   : true
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , series    : [seriesBarD1 {name: "Number of publication / year"} $
                 map (\n -> dataSerie {value: n, itemStyle : itemStyle {color:grey}}) count'] }

getMetrics :: Session -> Path -> Aff HistoMetrics
getMetrics session {corpusId, tabType} = do
  ChartMetrics ms <- get session chart
  pure ms."data"
  where chart = Chart {chartType: Histo, tabType: tabType} (Just corpusId)

histo :: Record Props -> R.Element
histo props = R.createElement histoCpt props []

histoCpt :: R.Component Props
histoCpt = R.hooksComponent "LoadedMetricsHisto" cpt
  where
    cpt {session,path} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadView session setReload path

metricsLoadView :: Session -> R.State Int -> Path -> R.Element
metricsLoadView s setReload p = R.createElement el {session: s, path: p} []
  where
    el = R.hooksComponent "MetricsLoadedHistoView" cpt
    cpt {path,session} _ = do
      useLoader path (getMetrics session) $ \loaded ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> HistoMetrics -> R.Element
loadedMetricsView setReload loaded = U.reloadButtonWrap setReload $ chart $ chartOptions loaded
