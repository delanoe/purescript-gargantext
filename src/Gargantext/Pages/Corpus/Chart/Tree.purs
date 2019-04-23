module Gargantext.Pages.Corpus.Chart.Tree where

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

import Gargantext.Pages.Corpus.Dashboard (treeMapEx)


type Path =
  { corpusId :: Int
  , listId   :: Int
  , tabType  :: TabType
  , limit    :: Maybe Int
  }


newtype Metrics = Metrics
  { "data" :: Array TreeNode
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .? "data"
    pure $ Metrics { "data": d }

type Loaded  = Array TreeNode

loadedMetricsSpec :: Spec {} (Loader.InnerProps Path Loaded ()) Void
loadedMetricsSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} (Loader.InnerProps Path Loaded ()) Void
    render dispatch {loaded} {} _ = [chart (scatterOptions loaded)]

scatterOptions :: Array TreeNode -> Options
scatterOptions nodes = Options
  { mainTitle : "Tree"
  , subTitle  : "Tree Sub Title"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position : "", show: false, min:0}
  , series    : [ mkTree TreeMap nodes]
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
-- TODO improve the formatter:
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-obama

  }

getMetrics :: Path -> Aff Loaded
getMetrics {corpusId, listId, limit, tabType} = do
  Metrics ms <- get $ toUrl Back (Chart {chartType : ChartTree, tabType: tabType}) $ Just corpusId
  pure ms."data"

metricsLoaderClass :: ReactClass (Loader.Props Path Loaded)
metricsLoaderClass = Loader.createLoaderClass "MetricsLoader" getMetrics

metricsLoader :: Loader.Props' Path Loaded -> ReactElement
metricsLoader props = createElement metricsLoaderClass props []

treeSpec :: Spec {} Path Void
treeSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Path Void
    render dispatch path {} _ =
      [ metricsLoader
        { path
        , component: createClass "LoadedMetrics" loadedMetricsSpec (const {})
        } ]
