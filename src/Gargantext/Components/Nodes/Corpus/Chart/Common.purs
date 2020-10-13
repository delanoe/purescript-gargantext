module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Types (Reload, Path, Props, MetricsProps, ReloadPath)
import Gargantext.Hooks.Loader (HashedResponse, useLoader, useLoaderWithCacheAPI)
import Gargantext.Utils.Crypto (Hash)
import Gargantext.Sessions (Session)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Corpus.Chart.Common"

type MetricsLoadViewProps a = (
    getMetrics :: Session -> ReloadPath -> Aff a
  , loaded :: Record MetricsProps -> a -> R.Element
  | MetricsProps
  )

cacheName :: String
cacheName = "metrics"

metricsLoadView :: forall a. Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []

metricsLoadViewCpt :: forall a. R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = R.hooksComponentWithModule thisModule "metricsLoadView" cpt
  where
    cpt { getMetrics, loaded, path, reload, session } _ = do
      useLoader (fst reload /\ path) (getMetrics session) $ \l ->
        loaded { path, reload, session } l

type MetricsWithCacheLoadViewProps res ret = (
    getMetricsHash :: Session -> ReloadPath -> Aff Hash
  , handleResponse :: HashedResponse res -> ret
  , loaded :: Record MetricsProps -> ret -> R.Element
  , mkRequest :: ReloadPath -> GUC.Request
  | MetricsProps
  )

metricsWithCacheLoadView :: forall res ret. DecodeJson res =>
                            Record (MetricsWithCacheLoadViewProps res ret) -> R.Element
metricsWithCacheLoadView p = R.createElement metricsWithCacheLoadViewCpt p []

metricsWithCacheLoadViewCpt :: forall res ret. DecodeJson res =>
                               R.Component (MetricsWithCacheLoadViewProps res ret)
metricsWithCacheLoadViewCpt = R.hooksComponentWithModule thisModule "metricsWithCacheLoadView" cpt
  where
    cpt { getMetricsHash, handleResponse, loaded, mkRequest, path, reload, session } _ = do
      useLoaderWithCacheAPI { cacheEndpoint: (getMetricsHash session)
                            , handleResponse
                            , mkRequest
                            , path: (fst reload /\ path)
                            , renderer: loaded { path, reload, session } }
