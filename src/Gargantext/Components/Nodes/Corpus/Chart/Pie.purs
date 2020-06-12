module Gargantext.Components.Nodes.Corpus.Chart.Pie where

import Prelude (bind, map, pure, ($), (>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Array (zip, filter)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (take, joinWith, Pattern(..), split, length)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (seriesBarD1, seriesPieD1)
import Gargantext.Components.Charts.Options.Color (blue)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (Path, Props)
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType)

newtype ChartMetrics = ChartMetrics
  { "data" :: HistoMetrics
  }

instance decodeChartMetrics :: DecodeJson ChartMetrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ ChartMetrics { "data": d }

newtype HistoMetrics = HistoMetrics
  { dates :: Array String
  , count :: Array Number
  }

instance decodeHistoMetrics :: DecodeJson HistoMetrics where
  decodeJson json = do
    obj   <- decodeJson json
    d <- obj .: "dates"
    c <- obj .: "count"
    pure $ HistoMetrics { dates : d , count: c}

type Loaded = HistoMetrics

chartOptionsBar :: HistoMetrics -> Options
chartOptionsBar (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Bar"
  , subTitle  : "Count of GraphTerm"
  , xAxis     : xAxis' $ map (\t -> joinWith " " $ map (take 3) $ A.take 3 $ filter (\s -> length s > 3) $ split (Pattern " ") t) dates'
  , yAxis     : yAxis' { position: "left", show: true, min:0}
  , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", itemStyle: itemStyle {color:blue}, value: n }) count']
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  }

chartOptionsPie :: HistoMetrics -> Options
chartOptionsPie (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Pie"
  , subTitle  : "Distribution by GraphTerm"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: false, min:0}
  , series    : [seriesPieD1 {name: "Data"} $ map (\(Tuple n v) -> dataSerie {name: n, value:v}) $ zip dates' count']
  -- , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", value: n }) count']
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  }


getMetrics :: Session -> Record Path -> Aff HistoMetrics
getMetrics session {corpusId, limit, listId, tabType} = do
  ChartMetrics ms <- get session chart
  pure ms."data"
  where chart = Chart {chartType: ChartPie, limit, listId, tabType} (Just corpusId)

pie :: Record Props -> R.Element
pie props = R.createElement pieCpt props []

pieCpt :: R.Component Props
pieCpt = R.hooksComponent "G.C.N.C.C.P.pie" cpt
  where
    cpt {path,session} _ = do
      reload <- R.useState' 0
      pure $ metricsLoadView {getMetrics, loaded: loadedPie, path, reload, session}

loadedPie :: Session -> Record Path -> R.State Int -> HistoMetrics -> R.Element
loadedPie _session _path setReload loaded =
  U.reloadButtonWrap setReload $ chart $ chartOptionsPie loaded


bar :: Record Props -> R.Element
bar props = R.createElement barCpt props []

barCpt :: R.Component Props
barCpt = R.hooksComponent "LoadedMetricsBar" cpt
  where
    cpt {path, session} _ = do
      reload <- R.useState' 0
      pure $ metricsLoadView {getMetrics, loaded: loadedBar, path, reload, session}

loadedBar :: Session -> Record Path -> R.State Int -> Loaded -> R.Element
loadedBar _session _path setReload loaded =
  U.reloadButtonWrap setReload $ chart $ chartOptionsBar loaded
