module Gargantext.Components.Nodes.Corpus.Chart.Tree where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Charts.Options.ECharts (Options(..), chart, xAxis', yAxis')
import Gargantext.Components.Charts.Options.Font (mkTooltip, templateFormatter)
import Gargantext.Components.Charts.Options.Series (TreeNode, Trees(..), mkTree)
import Gargantext.Components.Nodes.Corpus.Chart.Common (metricsWithCacheLoadView)
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, Path, Props, ReloadPath)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.Loader (HashedResponse(..))
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
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Tree"

newtype Metrics = Metrics {
    "data" :: Array TreeNode
  }
derive instance Generic Metrics _
derive instance Newtype Metrics _
derive newtype instance JSON.ReadForeign Metrics
derive newtype instance JSON.WriteForeign Metrics

type Loaded  = Array TreeNode

type LoadedProps =
  ( metrics :: Array TreeNode
  | MetricsProps )

scatterOptions :: Record LoadedProps -> Options
scatterOptions { onClick, onInit, metrics: nodes } = Options
  { mainTitle : "Tree"
  , subTitle  : "Tree Sub Title"
  , xAxis     : xAxis' []
  , yAxis     : yAxis' { position : "", show: false, min:0}
  , series    : [ mkTree TreeMap nodes]
  , addZoom   : false
  , tooltip   : mkTooltip { formatter: templateFormatter "{b0}" }
  , onClick
  , onInit
-- TODO improve the formatter:
-- https://ecomfe.github.io/echarts-examples/public/editor.html?c=treemap-obama

  }

getMetricsHash :: Session -> ReloadPath -> AffRESTError String
getMetricsHash session (_ /\ { corpusId, listId, tabType }) = do
  get session $ ChartHash { chartType: ChartTree, listId: mListId, tabType } (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

chartUrl :: Record Path -> SessionRoute
chartUrl { corpusId, limit, listId, tabType } = Chart {chartType: ChartTree, limit, listId: mListId, tabType} (Just corpusId)
  where
    mListId = if listId == 0 then Nothing else (Just listId)

handleResponse :: HashedResponse Metrics -> Loaded
handleResponse (HashedResponse { value: Metrics ms }) = ms."data"

mkRequest :: Session -> ReloadPath -> GUC.Request
mkRequest session (_ /\ path) = GUC.makeGetRequest session $ chartUrl path

tree :: Record Props -> R.Element
tree props = R.createElement treeCpt props []
treeCpt :: R.Component Props
treeCpt = here.component "tree" cpt
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

loaded :: R2.Leaf LoadedProps
loaded = R2.leaf loadedCpt
loadedCpt :: R.Component LoadedProps
loadedCpt = here.component "loaded" cpt where
  cpt p@{ path
        , reload
        , session } _ = do
    pure $ H.div {} [
      {-  U.reloadButton reload
  , U.chartUpdateButton { chartType: ChartTree, path, reload, session }
  , -} chart $ scatterOptions p
       ]
