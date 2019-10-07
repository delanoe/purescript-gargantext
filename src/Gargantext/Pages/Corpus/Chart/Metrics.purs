module Gargantext.Pages.Corpus.Chart.Metrics where

import Prelude (bind, negate, pure, ($), (<$>), (<>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Gargantext.Config.REST (get)
import Reactix as R
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, yAxis')
import Gargantext.Components.Charts.Options.Type (xAxis)
import Gargantext.Components.Charts.Options.Series (Series, seriesScatterD2)
import Gargantext.Components.Charts.Options.Color (green, grey, red)
import Gargantext.Components.Charts.Options.Font (itemStyle, mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Data (dataSerie)
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Pages.Corpus.Chart.Utils as U
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session)
import Gargantext.Types (TabType, TermList(..))

type Path
  = { corpusId :: Int
    , listId :: Int
    , tabType :: TabType
    , limit :: Maybe Int
    }

type Props
  = ( path :: Path, session :: Session )

newtype Metric
  = Metric
  { label :: String
  , x :: Number
  , y :: Number
  , cat :: TermList
  }

instance decodeMetric :: DecodeJson Metric where
  decodeJson json = do
    obj <- decodeJson json
    label <- obj .: "label"
    x <- obj .: "x"
    y <- obj .: "y"
    cat <- obj .: "cat"
    pure $ Metric { label, x, y, cat }

newtype Metrics
  = Metrics
  { "data" :: Array Metric
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d <- obj .: "data"
    pure $ Metrics { "data": d }

type Loaded
  = Array Metric

scatterOptions :: Array Metric -> Options
scatterOptions metrics' =
  Options
    { mainTitle: "Ngrams Selection Metrics"
    , subTitle: "Local metrics (Inc/Exc, Spe/Gen), Global metrics (TFICF maillage)"
    , xAxis: xAxis { min: -1 }
    , yAxis: yAxis' { position: "", show: true, min: -2 }
    , series: map2series $ metric2map metrics'
    , addZoom: false
    , tooltip: mkTooltip { formatter: templateFormatter "{b0}" }
    }
  where
  metric2map :: Array Metric -> Map TermList (Array Metric)
  metric2map ds = Map.fromFoldableWith (<>) $ (\(Metric m) -> Tuple m.cat [ Metric m ]) <$> ds

  --{-
  map2series :: Map TermList (Array Metric) -> Array Series
  map2series ms = toSeries <$> Map.toUnfoldable ms
    where
    -- TODO colors are not respected yet
    toSeries (Tuple k ms') = seriesScatterD2 { symbolSize: 5.0 } (toSerie color <$> ms')
      where
      color = case k of
        StopTerm -> red
        GraphTerm -> green
        CandidateTerm -> grey

      toSerie color' (Metric { label, x, y }) =
        dataSerie
          { name: label
          , itemStyle: itemStyle { color: color' }
          -- , label: {show: true}
          , value: [ x, y ]
          }

--}
getMetrics :: Session -> Path -> Aff Loaded
getMetrics session { corpusId, listId, limit, tabType } = do
  Metrics ms <- get $ url session metrics'
  pure ms."data"
  where
  metrics' = CorpusMetrics { listId, tabType, limit } (Just corpusId)

metrics :: Record Props -> R.Element
metrics props = R.createElement metricsCpt props []

metricsCpt :: R.Component Props
metricsCpt = R.hooksComponent "LoadedMetrics" cpt
  where
  cpt { path, session } _ = do
    setReload <- R.useState' 0
    pure $ metricsLoadView session setReload path

metricsLoadView :: Session -> R.State Int -> Path -> R.Element
metricsLoadView s setReload p = R.createElement el { session: s, path: p } []
  where
  el = R.hooksComponent "MetricsLoadedView" cpt

  cpt { session, path } _ = do
    useLoader path (getMetrics session)
      $ \loaded ->
          loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> Loaded -> R.Element
loadedMetricsView setReload loaded = U.reloadButtonWrap setReload $ chart $ scatterOptions loaded
