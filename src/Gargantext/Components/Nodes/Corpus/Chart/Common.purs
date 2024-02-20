module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, ReloadPath)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.Loader (HashedResponse, useLoader, useLoaderWithCacheAPI)
import Gargantext.Prelude
import Gargantext.Sessions (Session)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Simple.JSON as JSON
import Toestand as T


here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Common"

type MetricsLoadViewProps a = (
    getMetrics :: Session -> ReloadPath -> AffRESTError a
  , loaded :: Record MetricsProps -> a -> R.Element
  | MetricsProps
  )

cacheName :: String
cacheName = "metrics"

metricsLoadView :: forall a. Eq a => Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []
metricsLoadViewCpt :: forall a. Eq a => R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = R2.hereComponent here "metricsLoadView" hCpt where
  hCpt hp { getMetrics
          , loaded
          , onClick
          , onInit
          , path
          , reload
          , session } _ = do
    reload' <- T.useLive T.unequal reload

    useLoader { errorHandler: Nothing
              , herePrefix: hp
              , loader: getMetrics session
              , path: reload' /\ path
              , render: \l -> loaded { path, reload, session, onClick, onInit } l }

type MetricsWithCacheLoadViewProps res ret =
  ( getMetricsHash :: Session -> ReloadPath -> AffRESTError Hash
  , handleResponse :: HashedResponse res -> ret
  -- , loaded         :: Record MetricsProps -> ret -> R.Element
  , loaded         :: Record (metrics :: ret | MetricsProps) -> R.Element
  , mkRequest      :: ReloadPath -> GUC.Request
  | MetricsProps
  )

metricsWithCacheLoadView :: forall res ret.
                            Eq ret => JSON.ReadForeign res =>
                            Record (MetricsWithCacheLoadViewProps res ret) -> R.Element
metricsWithCacheLoadView p = R.createElement metricsWithCacheLoadViewCpt p []
metricsWithCacheLoadViewCpt :: forall res ret.
                               Eq ret => JSON.ReadForeign res =>
                               R.Component (MetricsWithCacheLoadViewProps res ret)
metricsWithCacheLoadViewCpt = here.component "metricsWithCacheLoadView" cpt
  where
    cpt { getMetricsHash
        , handleResponse
        , loaded
        , mkRequest
        , path
        , reload
        , session
        , onClick
        , onInit } _ = do
      reload' <- T.useLive T.unequal reload

      useLoaderWithCacheAPI { cacheEndpoint: (getMetricsHash session)
                            , handleResponse
                            , mkRequest
                            , path: (reload' /\ path)
                            , renderer: \metrics -> loaded { path, reload, session, onClick, onInit, metrics }
                            , spinnerClass: Just "echarts-for-react-spinner" }
