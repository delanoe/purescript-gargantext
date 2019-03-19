module Gargantext.Pages.Corpus.Metrics where

import Data.Array (foldl)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Map (Map)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Gargantext.Config -- (End(..), Path(..), TabType, toUrl)
import Gargantext.Config.REST (get)
import React (ReactClass, ReactElement, createElement)
import Thermite (Spec, Render, defaultPerformAction, simpleSpec, createClass)
import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Type
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Color
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data

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
    label <- obj .? "label"
    x     <- obj .? "x"
    y     <- obj .? "y"
    cat   <- obj .? "cat"
    pure $ Metric { label, x, y, cat }

newtype Metrics = Metrics
  { "data" :: Array Metric
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .? "data"
    pure $ Metrics { "data": d }

type Loaded  = Array Metric

loadedMetricsSpec :: Spec {} (Loader.InnerProps Path Loaded ()) Void
loadedMetricsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} (Loader.InnerProps Path Loaded ()) Void
    render dispatch {loaded} {} _ = [chart (scatterOptions loaded)]

scatterOptions :: Array Metric -> Options
scatterOptions metrics = Options
  { mainTitle : "Ngrams Selection Metrics"
  , subTitle  : "Inc/Exc, Spe/Gen, TFICF"
  , xAxis     : xAxis { min: 0 }
  , yAxis     : yAxis' { position : "", show: true }
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
        toSeries (Tuple k ms) =
            seriesScatterD2 {symbolSize: 5.0} (toSerie color <$> ms)
          where
            color =
              case k of
                StopTerm -> red
                GraphTerm -> green
                CandidateTerm -> grey
            toSerie color (Metric {label,x,y}) =
              dataSerie { name: label, itemStyle: itemStyle {color}
                     -- , label: {show: true}
                        , value: [x,y]
                        }
    --}

getMetrics :: Path -> Aff Loaded
getMetrics {corpusId, listId, limit, tabType} = do
  Metrics ms <- get $ toUrl Back (CorpusMetrics {listId, tabType, limit}) $ Just corpusId
  pure ms."data"

metricsLoaderClass :: ReactClass (Loader.Props Path Loaded)
metricsLoaderClass = Loader.createLoaderClass "MetricsLoader" getMetrics

metricsLoader :: Loader.Props' Path Loaded -> ReactElement
metricsLoader props = createElement metricsLoaderClass props []

metricsSpec :: Spec {} Path Void
metricsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Path Void
    render dispatch path {} _ =
      [ metricsLoader
        { path
        , component: createClass "LoadedMetrics" loadedMetricsSpec (const {})
        } ]
