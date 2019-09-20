module Gargantext.Pages.Corpus.Chart.Tree where

import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Config (Ends, BackendRoute(..), TabType, ChartType(..), url)
import Gargantext.Config.REST (get)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Types (TermList(..))
import Gargantext.Components.Charts.Options.ECharts
import Gargantext.Components.Charts.Options.Series
import Gargantext.Components.Charts.Options.Font
import Gargantext.Components.Charts.Options.Data
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Utils.Reactix as R2
import Gargantext.Pages.Corpus.Chart.Utils as U


type Path =
  { corpusId :: Int
  , listId   :: Int
  , tabType  :: TabType
  , limit    :: Maybe Int
  }
type Props = ( path :: Path, ends :: Ends )

newtype Metrics = Metrics
  { "data" :: Array TreeNode
  }

instance decodeMetrics :: DecodeJson Metrics where
  decodeJson json = do
    obj <- decodeJson json
    d   <- obj .: "data"
    pure $ Metrics { "data": d }

type Loaded  = Array TreeNode

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

getMetrics :: Ends -> Path -> Aff Loaded
getMetrics ends {corpusId, listId, limit, tabType} = do
  Metrics ms <- get $ url ends chart
  pure ms."data"
  where
    chart = Chart {chartType : ChartTree, tabType: tabType} (Just corpusId)

tree :: Record Props -> R.Element
tree props = R.createElement treeCpt props []

treeCpt :: R.Component Props
treeCpt = R.hooksComponent "LoadedMetrics" cpt
  where
    cpt {path, ends} _ = do
      setReload <- R.useState' 0
      pure $ metricsLoadView ends setReload path

metricsLoadView :: Ends -> R.State Int -> Path -> R.Element
metricsLoadView ends setReload p = R.createElement el p []
  where
    el = R.hooksComponent "MetricsLoadView" cpt
    cpt p _ = do
      useLoader p (getMetrics ends) $ \loaded ->
        loadedMetricsView setReload loaded

loadedMetricsView :: R.State Int -> Loaded -> R.Element
loadedMetricsView setReload loaded =
  H.div {}
  [ U.reloadButton setReload
  , chart (scatterOptions loaded) ]
