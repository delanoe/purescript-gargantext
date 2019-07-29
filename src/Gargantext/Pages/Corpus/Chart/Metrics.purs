module Gargantext.Pages.Corpus.Chart.Metrics where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Gargantext.Config
import Gargantext.Config.REST (get)
import Reactix as R

import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Type
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Color
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data
import Gargantext.Utils.Reactix as R2
import Gargantext.Pages.Corpus.Chart.Utils as U

type Path =
  { corpusId :: Int
  , listId   :: Int
  , tabType  :: TabType
  , limit    :: Maybe Int
  }

newtype Metric = Metric
  { label :: String
  , x     :: Number
  , y     :: Number
  , cat   :: TermList
  }

instance decodeMetric :: DecodeJson Metric where
  decodeJson json = do
    obj   <- decodeJson json
    label <- obj .: "label"
    x     <- obj .: "x"
    y     <- obj .: "y"
    cat   <- obj .: "cat"
    pure $ Metric { label, x, y, cat }

newtype Metrics = Metrics
  { "data" :: Array Metric
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ Metrics { "data": d }

type Loaded  = Array Metric

scatterOptions :: Array Metric -> Options
scatterOptions metrics = Options
  { mainTitle : "Ngrams Selection Metrics"
  , subTitle  : "Local metrics (Inc/Exc, Spe/Gen), Global metrics (TFICF maillage)"
  , xAxis     : xAxis { min: -1 }
  , yAxis     : yAxis' { position : "", show: true, min : -2}
  , series    : map2series $ metric2map metrics
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
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
                GraphTerm -> green
                CandidateTerm -> grey
            toSerie color' (Metric {label,x,y}) =
              dataSerie { name: label, itemStyle: itemStyle {color: color'}
                     -- , label: {show: true}
                        , value: [x,y]
                        }
    --}

getMetrics :: Path -> Aff Loaded
getMetrics {corpusId, listId, limit, tabType} = do
  Metrics ms <- get $ toUrl endConfigStateful Back (CorpusMetrics {listId, tabType, limit}) $ Just corpusId
  pure ms."data"


metricsSpec = R2.elSpec $ R.hooksComponent "LoadedMetrics" cpt
  where
    cpt p _ = do
      setReload <- R.useState' 0

      pure $ metricsLoadView setReload p

metricsLoadView :: R.State Int -> Path -> R.Element
metricsLoadView setReload p = R.createElement el p []
  where
    el = R.hooksComponent "MetricsLoadedView" cpt
    cpt p' _ = do
      useLoader p' getMetrics $ \{loaded} ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> Loaded -> R.Element
loadedMetricsView setReload loaded = U.reloadButtonWrap setReload $ R2.buff $ chart $ scatterOptions loaded
