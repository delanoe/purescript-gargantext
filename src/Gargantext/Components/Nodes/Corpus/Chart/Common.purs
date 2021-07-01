module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Simple.JSON as JSON
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Types (MetricsProps, ReloadPath)
import Gargantext.Hooks.Loader (HashedResponse, useLoader, useLoaderWithCacheAPI)
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Sessions (Session)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Chart.Common"

type MetricsLoadViewProps a = (
    getMetrics :: Session -> ReloadPath -> Aff a
  , loaded :: Record MetricsProps -> a -> R.Element
  | MetricsProps
  )

cacheName :: String
cacheName = "metrics"

metricsLoadView :: forall a. Eq a => Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []

metricsLoadViewCpt :: forall a. Eq a => R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = here.component "metricsLoadView" cpt
  where
    cpt { getMetrics, loaded, path, reload, session } _ = do
      reload' <- T.useLive T.unequal reload

      useLoader (reload' /\ path) (getMetrics session) $ \l ->
        loaded { path, reload, session } l

type MetricsWithCacheLoadViewProps res ret = (
    getMetricsHash :: Session -> ReloadPath -> Aff Hash
  , handleResponse :: HashedResponse res -> ret
  , loaded :: Record MetricsProps -> ret -> R.Element
  , mkRequest :: ReloadPath -> GUC.Request
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
    cpt { getMetricsHash, handleResponse, loaded, mkRequest, path, reload, session } _ = do
      reload' <- T.useLive T.unequal reload

      useLoaderWithCacheAPI { cacheEndpoint: (getMetricsHash session)
                            , handleResponse
                            , mkRequest
                            , path: (reload' /\ path)
                            , renderer: loaded { path, reload, session } }
