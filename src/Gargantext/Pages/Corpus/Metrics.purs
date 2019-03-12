module Gargantext.Pages.Corpus.Metrics where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Config -- (End(..), Path(..), TabType, toUrl)
import Gargantext.Config.REST (get)
import React (ReactClass, ReactElement, createElement)
import Thermite (Spec, Render, defaultPerformAction, simpleSpec, createClass)
import Gargantext.Prelude
import Gargantext.Types (TermList)
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Series

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

scatterOptions :: Loaded -> Options
scatterOptions ds = Options
  { mainTitle : "TODO Scatter test"
  , subTitle  : "TODO Scatter subtitle"
  , xAxis     : xAxis [] -- $ SeriesD2 $ seriesD2 Scatter 5.0 (_.x <$> ds)
  , yAxis     : [ SeriesD2 $ seriesD2 Scatter 5.0 (_y <$> ds) ]
  , yAxisFormat : (YAxisFormat { position : ""
                               , visible  : true
                               })
  , addZoom     : false
  }
  where
    _y (Metric {x,y}) = [x,y]

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
