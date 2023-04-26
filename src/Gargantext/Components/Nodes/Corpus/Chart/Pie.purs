module Gargantext.Components.Nodes.Corpus.Chart.Pie where

import Data.Array (zip, filter)
import Data.Array as A
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (take, joinWith, Pattern(..), split, length)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Charts.Options.Color (blue)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Series (seriesBarD1, seriesPieD1)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Prelude (class Eq, bind, map, pure, ($), (==), (>))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (ChartType(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Pie"

newtype ChartMetrics = ChartMetrics {
    "data" :: HistoMetrics
  }
derive instance Generic ChartMetrics _
derive instance Newtype ChartMetrics _
derive newtype instance JSON.ReadForeign ChartMetrics

newtype HistoMetrics = HistoMetrics
  { dates :: Array String
  , count :: Array Number
  }
derive instance Generic HistoMetrics _
derive instance Newtype HistoMetrics _
instance Eq HistoMetrics where eq = genericEq
derive newtype instance JSON.ReadForeign HistoMetrics
derive newtype instance JSON.WriteForeign HistoMetrics

type Loaded = HistoMetrics

type LoadedProps =
  ( metrics :: HistoMetrics
  | MetricsProps )

chartOptionsBar :: Record LoadedProps -> Options
chartOptionsBar { onClick
                , onInit
                , metrics: HistoMetrics { dates: dates', count: count'} } = Options
  { mainTitle : "Bar"
  , subTitle  : "Count of MapTerm"
  , xAxis     : xAxis' $ map (\t -> joinWith " " $ map (take 3) $ A.take 3 $ filter (\s -> length s > 3) $ split (Pattern " ") t) dates'
  , yAxis     : yAxis' { position: "left", show: true, min:0}
  , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", itemStyle: itemStyle {color:blue}, value: n }) count']
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , onClick
  , onInit
  }

chartOptionsPie :: Record LoadedProps -> Options
chartOptionsPie { onClick
                , onInit
                , metrics: HistoMetrics { dates: dates', count: count'} } = Options
  { mainTitle : "Pie"
  , subTitle  : "Distribution by MapTerm"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position: "", show: false, min:0}
  , series    : [seriesPieD1 {name: "Data"} $ map (\(Tuple n v) -> dataSerie {name: n, value:v}) $ zip dates' count']
  -- , series    : [seriesBarD1 {name: "Number of publication / year"} $ map (\n -> dataSerie {name: "", value: n }) count']
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , onClick
  , onInit
  }

getMetricsHash :: Session -> ReloadPath -> AffRESTError String
getMetricsHash session (_ /\ { corpusId, listId, tabType }) = do
  get session $ ChartHash { chartType: ChartPie, listId: mListId, tabType } (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: ChartPie, limit, listId: mListId, tabType} (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

handleResponse :: HashedResponse ChartMetrics -> HistoMetrics
handleResponse (HashedResponse { value: ChartMetrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path) = GUC.makeGetRequest session $ chartUrl path

pie :: R2.Leaf Props
pie = R2.leaf pieCpt
pieCpt :: R.Component Props
pieCpt = here.component "pie" cpt
  where
    cpt { boxes, path, session, onClick, onInit } _ = do
      reload <- T.useBox T2.newReload

      pure $ metricsWithCacheLoadView
        { boxes
        , getMetricsHash
        , handleResponse
        , loaded: loadedPie
        , mkRequest: mkRequest session
        , path
        , reload
        , session
        , onClick
        , onInit
        }

loadedPie :: R2.Leaf LoadedProps
loadedPie = R2.leaf loadedPieCpt
loadedPieCpt :: R.Component LoadedProps
loadedPieCpt = here.component "loadedPie" cpt where
  cpt p@{ path
        , reload
        , session } _ = do
    pure $ H.div {} [
      {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartPie, path, reload, session }
  , -} chart $ chartOptionsPie p
       ]


bar :: Record Props -> R.Element
bar props = R.createElement barCpt props []
barCpt :: R.Component Props
barCpt = here.component "bar" cpt
  where
    cpt { boxes, path, session, onClick, onInit} _ = do
      reload <- T.useBox T2.newReload

      pure $ metricsWithCacheLoadView {
           boxes
         , getMetricsHash
         , handleResponse
         , loaded: loadedBar
         , mkRequest: mkRequest session
         , path
         , reload
         , session
         , onClick
         , onInit
         }

loadedBar :: R2.Leaf LoadedProps
loadedBar = R2.leaf loadedBarCpt
loadedBarCpt :: R.Component LoadedProps
loadedBarCpt = here.component "loadedBar" cpt where
  cpt p@{ path
        , reload
        , session } _ = do
    pure $ H.div {} [
      {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartBar, path, reload, session }
  , -} chart $ chartOptionsBar p
       ]
