module Gargantext.Components.Nodes.Corpus.Chart.Pie where

import Prelude (bind, map, pure, ($), (>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Array (zip, filter)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (take, joinWith, Pattern(..), split, length)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Gargantext.Config.REST (get)
import Reactix as R

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (seriesBarD1, seriesPieD1)
import Gargantext.Components.Charts.Options.Color (blue)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session)
import Gargantext.Types (ChartType(..), TabType)

type Path =
  { corpusId :: Int
  , tabType  :: TabType
  }

type Props = ( session :: Session, path :: Path )

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


getMetrics :: Session -> Path -> Aff HistoMetrics
getMetrics session {corpusId, tabType:tabType} = do
  ChartMetrics ms <- get $ url session chart
  pure ms."data"
  where chart = Chart {chartType: ChartPie, tabType: tabType} (Just corpusId)

pie :: Record Props -> R.Element
pie props = R.createElement pieCpt props []

pieCpt :: R.Component Props
pieCpt = R.hooksComponent "LoadedMetricsPie" cpt
  where
    cpt {path,session} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadPieView session setReload path

metricsLoadPieView :: Session -> R.State Int -> Path -> R.Element
metricsLoadPieView s setReload p = R.createElement el {session: s,path: p} []
  where
    el = R.hooksComponent "MetricsLoadedPieView" cpt
    cpt {session,path} _ = do
      useLoader path (getMetrics session) $ \loaded ->
        loadedMetricsPieView setReload loaded

loadedMetricsPieView :: R.State Int -> HistoMetrics -> R.Element
loadedMetricsPieView setReload loaded = U.reloadButtonWrap setReload $ chart $ chartOptionsPie loaded


bar :: Record Props -> R.Element
bar props = R.createElement barCpt props []

barCpt :: R.Component Props
barCpt = R.hooksComponent "LoadedMetricsBar" cpt
  where
    cpt {path, session} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadBarView session setReload path


metricsLoadBarView :: Session -> R.State Int -> Path -> R.Element
metricsLoadBarView s setReload p = R.createElement el {path: p, session: s} []
  where
    el = R.hooksComponent "MetricsLoadedBarView" cpt
    cpt {path, session} _ = do
      useLoader path (getMetrics session) $ \loaded ->
        loadedMetricsBarView setReload loaded

loadedMetricsBarView :: R.State Int -> Loaded -> R.Element
loadedMetricsBarView setReload loaded = U.reloadButtonWrap setReload $ chart $ chartOptionsBar loaded
