module Gargantext.Pages.Corpus.Chart.Pie where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Array (zip, filter)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (take, joinWith, Pattern(..), split, length)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Gargantext.Config (Ends, BackendRoute(..), TabType, ChartType(..), url)
import Gargantext.Config.REST (get)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Series (seriesBarD1, seriesPieD1)
import Gargantext.Components.Charts.Options.Color (blue)
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Utils.Reactix as R2
import Gargantext.Pages.Corpus.Chart.Utils as U

type Path =
  { corpusId :: Int
  , tabType  :: TabType
  }

type Props = ( ends :: Ends, path :: Path )

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


getMetrics :: Ends -> Path -> Aff HistoMetrics
getMetrics ends {corpusId, tabType:tabType} = do
  ChartMetrics ms <- get $ url ends chart
  pure ms."data"
  where chart = Chart {chartType: ChartPie, tabType: tabType} (Just corpusId)

pie :: Record Props -> R.Element
pie props = R.createElement pieCpt props []

pieCpt :: R.Component Props
pieCpt = R.hooksComponent "LoadedMetricsPie" cpt
  where
    cpt {path,ends} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadPieView ends setReload path

metricsLoadPieView :: Ends -> R.State Int -> Path -> R.Element
metricsLoadPieView ends setReload path = R.createElement el {ends,path} []
  where
    el = R.hooksComponent "MetricsLoadedPieView" cpt
    cpt {ends,path} _ = do
      useLoader path (getMetrics ends) $ \loaded ->
        loadedMetricsPieView setReload loaded

loadedMetricsPieView :: R.State Int -> HistoMetrics -> R.Element
loadedMetricsPieView setReload loaded = U.reloadButtonWrap setReload $ chart $ chartOptionsPie loaded


bar :: Record Props -> R.Element
bar props = R.createElement barCpt props []

barCpt :: R.Component Props
barCpt = R.hooksComponent "LoadedMetricsBar" cpt
  where
    cpt {path, ends} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadBarView ends setReload path


metricsLoadBarView :: Ends -> R.State Int -> Path -> R.Element
metricsLoadBarView ends setReload path = R.createElement el {ends,path} []
  where
    el = R.hooksComponent "MetricsLoadedBarView" cpt
    cpt {path, ends} _ = do
      useLoader path (getMetrics ends) $ \loaded ->
        loadedMetricsBarView setReload loaded

loadedMetricsBarView :: R.State Int -> Loaded -> R.Element
loadedMetricsBarView setReload loaded = U.reloadButtonWrap setReload $ chart $ chartOptionsBar loaded
