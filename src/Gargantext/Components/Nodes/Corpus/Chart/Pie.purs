module Gargantext.Components.Nodes.Corpus.Chart.Pie where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Array (zip, filter)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (take, joinWith, Pattern(..), split, length)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Series (seriesBarD1, seriesPieD1)
import Gargantext.Components.Charts.Options.Color (blue)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsLoadView, metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType)
import Gargantext.Utils.CacheAPI as GUC

newtype ChartMetrics = ChartMetrics {
    "data" :: HistoMetrics
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

instance encodeHistoMetrics :: EncodeJson HistoMetrics where
  encodeJson (HistoMetrics { dates, count }) =
       "count" := encodeJson count
    ~> "dates"    := encodeJson dates
    ~> jsonEmptyObject

type Loaded = HistoMetrics

chartOptionsBar :: HistoMetrics -> Options
chartOptionsBar (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Bar"
  , subTitle  : "Count of MapTerm"
  , xAxis     : xAxis' $ map (\t -> joinWith " " $ map (take 3) $ A.take 3 $ filter (\s -> length s > 3) $ split (Pattern " ") t) dates'
  , yAxis     : yAxis' { position: "left", show: true, min:0}
  , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", itemStyle: itemStyle {color:blue}, value: n }) count']
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  }

chartOptionsPie :: HistoMetrics -> Options
chartOptionsPie (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Pie"
  , subTitle  : "Distribution by MapTerm"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: false, min:0}
  , series    : [seriesPieD1 {name: "Data"} $ map (\(Tuple n v) -> dataSerie {name: n, value:v}) $ zip dates' count']
  -- , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", value: n }) count']
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  }


-- getMetrics :: Session -> Tuple Reload (Record Path) -> Aff (HashedResponse HistoMetrics)
-- getMetrics session (_ /\ { corpusId, limit, listId, tabType }) = do
--   HashedResponse { md5, value: ChartMetrics ms } <- GUC.get session chart --get session chart
--   pure $ HashedResponse { md5, value: ms."data" }
--   where chart = Chart {chartType: ChartPie, limit, listId, tabType} (Just corpusId)

getMetricsMD5 :: Session -> Tuple Reload (Record Path) -> Aff String
getMetricsMD5 session (_ /\ { corpusId, limit, listId, tabType }) = do
  get session $ ChartMD5 { chartType: ChartPie, listId: mListId, tabType } (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: ChartPie, limit, listId: mListId, tabType} (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

handleResponse :: HashedResponse ChartMetrics -> HistoMetrics
handleResponse (HashedResponse { value: ChartMetrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path@{ corpusId, limit, listId, tabType }) = GUC.makeGetRequest session $ chartUrl path

pie :: Record Props -> R.Element
pie props = R.createElement pieCpt props []

pieCpt :: R.Component Props
pieCpt = R.hooksComponent "G.C.N.C.C.P.pie" cpt
  where
    cpt { path, session } _ = do
      reload <- R.useState' 0
      pure $ metricsWithCacheLoadView {
          getMetricsMD5
        , handleResponse
        , loaded: loadedPie
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        }

loadedPie :: Record MetricsProps -> HistoMetrics -> R.Element
loadedPie { path, reload, session } loaded =
  H.div {} [
    U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartPie, path, reload, session }
  , chart $ chartOptionsPie loaded
  ]


bar :: Record Props -> R.Element
bar props = R.createElement barCpt props []

barCpt :: R.Component Props
barCpt = R.hooksComponent "LoadedMetricsBar" cpt
  where
    cpt {path, session} _ = do
      reload <- R.useState' 0
      --pure $ metricsLoadView {getMetrics, loaded: loadedBar, path, reload, session}
      pure $ metricsWithCacheLoadView {
          getMetricsMD5
        , handleResponse
        , loaded: loadedPie
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        }

loadedBar :: Record MetricsProps -> Loaded -> R.Element
loadedBar { path, reload, session } loaded =
  H.div {} [
    U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartBar, path, reload, session }
  , chart $ chartOptionsBar loaded
  ]
