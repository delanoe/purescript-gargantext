module Gargantext.Components.Nodes.Corpus.Chart.Histo where

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

import Gargantext.Components.Charts.Options.Color (grey, blue)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Series (seriesBarD1)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Config.REST (RESTError)
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Prelude (class Eq, bind, map, pure, ($), (==))
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
derive instance Newtype ChartMetrics _
instance Eq ChartMetrics where eq = genericEq
derive newtype instance JSON.ReadForeign ChartMetrics

newtype HistoMetrics = HistoMetrics { dates :: Array String, count :: Array Number }
derive instance Generic HistoMetrics _
derive instance Newtype HistoMetrics _
instance Eq HistoMetrics where eq = genericEq
derive newtype instance JSON.ReadForeign HistoMetrics
derive newtype instance JSON.WriteForeign HistoMetrics

type Loaded = HistoMetrics

chartOptions :: Record MetricsProps -> HistoMetrics -> Options
chartOptions { onClick, onInit } (HistoMetrics { dates: dates', count: count'}) = Options
  { mainTitle : "Histogram"
  , subTitle  : "Distribution of publications over time"
  , xAxis     : xAxis' dates'
  , yAxis     : yAxis' { position: "left", show: true, min:0}
  , addZoom   : true
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , series
  , onClick
  , onInit
  }
    where
      mapSeriesBar n = dataSerie
        { value: n
        , itemStyle: itemStyle { color: grey }
        , emphasis: { itemStyle: itemStyle { color: blue } }
        -- @XXX "select" action not working
        -- , selectedMode: selectedMode Single
        -- , select: { itemStyle: itemStyle { color: green }}
        }

      series =
        [ seriesBarD1 {name: "Number of publication / year"} $
          map mapSeriesBar count'
        ]

getMetricsHash :: Session -> ReloadPath -> Aff (Either RESTError String)
getMetricsHash session (_ /\ { corpusId, listId, tabType }) = do
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
mkRequest session (_ /\ path) = GUC.makeGetRequest session $ chartUrl path

histo :: Record Props -> R.Element
histo props = R.createElement histoCpt props []
histoCpt :: R.Component Props
histoCpt = here.component "histo" cpt
  where
    cpt { boxes, path, session, onClick, onInit } _ = do
      reload <- T.useBox T2.newReload

      pure $ metricsWithCacheLoadView
        { boxes
        , getMetricsHash
        , handleResponse
        , loaded
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        , onClick
        , onInit
        }

loaded :: Record MetricsProps -> HistoMetrics -> R.Element
loaded p l =
  H.div {} [
  {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: Histo, path, reload, session }
  , -} chart $ chartOptions p l
  ]
  -- TODO: parametrize ngramsType above
