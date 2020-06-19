module Gargantext.Components.Nodes.Corpus.Chart.Histo where

import Prelude (bind, map, pure, ($))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (seriesBarD1)
import Gargantext.Components.Charts.Options.Color (grey)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path, Props)
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType(..))

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

getMetrics :: Session -> Record Path -> Aff HistoMetrics
getMetrics session {corpusId, limit, listId, tabType} = do
  ChartMetrics ms <- get session chart
  pure ms."data"
  where
    chart = Chart {chartType: Histo, listId, tabType, limit} (Just corpusId)

histo :: Record Props -> R.Element
histo props = R.createElement histoCpt props []

histoCpt :: R.Component Props
histoCpt = R.hooksComponent "G.C.N.C.C.H.histo" cpt
  where
    cpt {path, session} _ = do
      reload <- R.useState' 0
      pure $ metricsLoadView {getMetrics, loaded, path, reload, session}

loaded :: Session -> Record Path -> R.State Int -> HistoMetrics -> R.Element
loaded session path reload loaded =
  H.div {} [
    U.reloadButton reload
  , U.chartUpdateButton { chartType: Histo, path, reload, session }
  , chart $ chartOptions loaded
  ]
  -- TODO: parametrize ngramsType above
