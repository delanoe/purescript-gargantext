module Gargantext.Components.Nodes.Corpus.Chart.Metrics where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Charts.Options.Color (green, grey, red)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, yAxis')
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Series (Series, seriesScatterD2)
import Gargantext.Components.Charts.Options.Type (xAxis)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.Loader (HashedResponse(..))
import Gargantext.Prelude (class Eq, bind, negate, pure, ($), (<$>), (<>))
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (TermList(..))
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Metrics"

newtype Metric = Metric
  { label :: String
  , x     :: Number
  , y     :: Number
  , cat   :: TermList
  }
derive instance Generic Metric _
derive instance Newtype Metric _
instance Eq Metric where eq = genericEq
derive newtype instance JSON.ReadForeign Metric
derive newtype instance JSON.WriteForeign Metric

newtype Metrics = Metrics {
     "data" :: Array Metric
  }
derive instance Generic Metrics _
derive instance Newtype Metrics _
derive newtype instance JSON.ReadForeign Metrics

type Loaded  = Array Metric

type LoadedProps =
  ( metrics :: Array Metric
  | MetricsProps )

scatterOptions :: Record LoadedProps -> Options
scatterOptions { onClick, onInit, metrics: metrics' } = Options
  { mainTitle : "Ngrams Selection Metrics"
  , subTitle  : "Local metrics (Inc/Exc, Spe/Gen), Global metrics (TFICF maillage)"
  , xAxis     : xAxis { min: -1 }
  , yAxis     : yAxis' { position : "", show: true, min : -2}
  , series    : map2series $ metric2map metrics'
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , onClick
  , onInit
  }
  where
    metric2map :: Array Metric -> Map TermList (Array Metric)
    metric2map ds = Map.fromFoldableWith (<>) $ (\(Metric m) -> Tuple m.cat [Metric m]) <$> ds

    --{-
    map2series :: Map TermList (Array Metric) -> Array Series
    map2series ms = toSeries <$> Map.toUnfoldable ms
      where
        -- TODO colors are not respected yet
        toSeries (Tuple k ms') =
            seriesScatterD2 {symbolSize: 5.0} (toSerie color <$> ms')
          where
            color =
              case k of
                StopTerm -> red
                MapTerm -> green
                CandidateTerm -> grey
            toSerie color' (Metric {label,x,y}) =
              dataSerie { name: label, itemStyle: itemStyle {color: color'}
                     -- , label: {show: true}
                        , value: [x,y]
                        }
    --}

getMetricsHash :: Session -> ReloadPath -> AffRESTError String
getMetricsHash session (_ /\ { corpusId, listId, tabType }) =
  get session $ CorpusMetricsHash { listId, tabType } (Just corpusId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = CorpusMetrics { limit, listId, tabType } (Just corpusId)

handleResponse :: HashedResponse Metrics -> Loaded
handleResponse (HashedResponse { value: Metrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path) = GUC.makeGetRequest session $ chartUrl path

metrics :: Record Props -> R.Element
metrics props = R.createElement metricsCpt props []
metricsCpt :: R.Component Props
metricsCpt = here.component "etrics" cpt
  where
    cpt { boxes, onClick, onInit, path, session } _ = do
      reload <- T.useBox T2.newReload

      pure $ metricsWithCacheLoadView {
          boxes
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


loaded :: R2.Leaf LoadedProps
loaded = R2.leaf loadedCpt
loadedCpt :: R.Component LoadedProps
loadedCpt = here.component "loaded" cpt where
  cpt p@{ path
        , reload
        , session } _ = do
    pure $ H.div {} [
      {-  U.reloadButton reload
      , U.chartUpdateButton { chartType: Scatter, path, reload, session }
      , -} chart $ scatterOptions p
      ]
