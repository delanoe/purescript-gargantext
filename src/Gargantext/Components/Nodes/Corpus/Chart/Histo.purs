module Gargantext.Components.Nodes.Corpus.Chart.Histo where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Charts.Options.Color (grey)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Series (seriesBarD1)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsLoadView, metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types
import Gargantext.Components.Nodes.Corpus.Chart.Utils as U
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Prelude
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..), TabType(..))
import Gargantext.Utils.CacheAPI as GUC
import Reactix as R
import Reactix.DOM.HTML as H

newtype ChartMetrics = ChartMetrics {
    "data" :: HistoMetrics
   }

instance decodeChartMetrics :: DecodeJson ChartMetrics where
  decodeJson json = do
    obj <- decodeJson json
    d <- obj .: "data"
    pure $ ChartMetrics { "data": d }

newtype HistoMetrics = HistoMetrics { dates :: Array String, count :: Array Number }

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

-- getMetrics :: Session -> Tuple Reload (Record Path) -> Aff (HashedResponse HistoMetrics)
-- getMetrics session (_ /\ { corpusId, limit, listId, tabType }) = do
--   HashedResponse { hash, value: ChartMetrics ms } <- get session chart
--   pure $ HashedResponse { hash, value: ms."data" }
--   where
--     chart = Chart {chartType: Histo, listId, tabType, limit} (Just corpusId)

getMetricsHash :: Session -> Tuple Reload (Record Path) -> Aff String
getMetricsHash session (_ /\ { corpusId, limit, listId, tabType }) = do
  get session $ ChartHash { chartType: Histo, listId, tabType } (Just corpusId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: Histo, limit, listId, tabType} (Just corpusId)

handleResponse :: HashedResponse ChartMetrics -> HistoMetrics
handleResponse (HashedResponse { value: ChartMetrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path@{ corpusId, limit, listId, tabType }) = GUC.makeGetRequest session $ chartUrl path

histo :: Record Props -> R.Element
histo props = R.createElement histoCpt props []

histoCpt :: R.Component Props
histoCpt = R.hooksComponent "G.C.N.C.C.H.histo" cpt
  where
    cpt { path, session } _ = do
      reload <- R.useState' 0
      pure $ metricsWithCacheLoadView {
          getMetricsHash
        , handleResponse
        , loaded
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        }

loaded :: Record MetricsProps -> HistoMetrics -> R.Element
loaded { path, reload, session } loaded =
  H.div {} [
    U.reloadButton reload
  , U.chartUpdateButton { chartType: Histo, path, reload, session }
  , chart $ chartOptions loaded
  ]
  -- TODO: parametrize ngramsType above
