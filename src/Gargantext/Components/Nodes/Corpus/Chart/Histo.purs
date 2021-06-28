module Gargantext.Components.Nodes.Corpus.Chart.Histo where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude (class Eq, bind, map, pure, ($), (==))

import Gargantext.Components.Charts.Options.Color (grey)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Series (seriesBarD1)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Histo"

newtype ChartMetrics = ChartMetrics {
    "data" :: HistoMetrics
   }
derive instance Generic ChartMetrics _
instance Eq ChartMetrics where
  eq = genericEq
instance DecodeJson ChartMetrics where
  decodeJson json = do
    obj <- decodeJson json
    d <- obj .: "data"
    pure $ ChartMetrics { "data": d }

newtype HistoMetrics = HistoMetrics { dates :: Array String, count :: Array Number }
derive instance Generic HistoMetrics _
instance Eq HistoMetrics where
  eq = genericEq
instance DecodeJson HistoMetrics where
  decodeJson json = do
    obj   <- decodeJson json
    d <- obj .: "dates"
    c <- obj .: "count"
    pure $ HistoMetrics { dates : d , count: c}
instance EncodeJson HistoMetrics where
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

getMetricsHash :: Session -> ReloadPath -> Aff String
getMetricsHash session (_ /\ { corpusId, limit, listId, tabType }) = do
  get session $ ChartHash { chartType: Histo, listId: mListId, tabType } (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: Histo, limit, listId: mListId, tabType} (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

handleResponse :: HashedResponse ChartMetrics -> HistoMetrics
handleResponse (HashedResponse { value: ChartMetrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path@{ corpusId, limit, listId, tabType }) = GUC.makeGetRequest session $ chartUrl path

histo :: Record Props -> R.Element
histo props = R.createElement histoCpt props []

histoCpt :: R.Component Props
histoCpt = here.component "histo" cpt
  where
    cpt { path, session } _ = do
      reload <- T.useBox T2.newReload

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
loaded { path, reload, session } l =
  H.div {} [
  {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: Histo, path, reload, session }
  , -} chart $ chartOptions l
  ]
  -- TODO: parametrize ngramsType above
