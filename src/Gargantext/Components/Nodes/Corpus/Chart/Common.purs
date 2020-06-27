module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Types
import Gargantext.Hooks.Loader (HashedResponse, useLoader, useLoaderWithCache)
import Gargantext.Sessions (Session)

type MetricsLoadViewProps a = (
    getMetrics :: Session -> Tuple Reload (Record Path) -> Aff a
  , loaded :: Session -> Record Path -> R.State Reload -> a -> R.Element
  | MetricsProps
  )

metricsLoadView :: forall a. Record (MetricsLoadViewProps a) -> R.Element
metricsLoadView p = R.createElement metricsLoadViewCpt p []

metricsLoadViewCpt :: forall a. R.Component (MetricsLoadViewProps a)
metricsLoadViewCpt = R.hooksComponent "G.C.N.C.C.metricsLoadView" cpt
  where
    cpt { getMetrics, loaded, path, reload, session } _ = do
      useLoader (fst reload /\ path) (getMetrics session) $ \l ->
        loaded session path reload l

type MetricsWithCacheLoadViewProps a = (
    keyFunc :: Tuple Reload (Record Path) -> String
  , getMetrics :: Session -> Tuple Reload (Record Path) -> Aff (HashedResponse a)
  , getMetricsMD5 :: Session -> Tuple Reload (Record Path) -> Aff String
  , loaded :: Session -> Record Path -> R.State Reload -> a -> R.Element
  | MetricsProps
  )

metricsWithCacheLoadView :: forall a. DecodeJson a => EncodeJson a =>
                            Record (MetricsWithCacheLoadViewProps a) -> R.Element
metricsWithCacheLoadView p = R.createElement metricsWithCacheLoadViewCpt p []

metricsWithCacheLoadViewCpt :: forall a. DecodeJson a => EncodeJson a => R.Component (MetricsWithCacheLoadViewProps a)
metricsWithCacheLoadViewCpt = R.hooksComponent "G.C.N.C.C.metricsWithCacheLoadView" cpt
  where
    cpt { getMetrics, getMetricsMD5, keyFunc, loaded, path, reload, session } _ = do
      useLoaderWithCache (fst reload /\ path) (metricsKeyFunc keyFunc) (getMetricsMD5 session) (getMetrics session) $ \l ->
        loaded session path reload l
    metricsKeyFunc keyFunc st@(_ /\ { corpusId, listId, tabType }) =
     "metrics-" <> (show tabType) <> "-" <> (show corpusId) <> "-" <> (show listId) <> "--" <> (keyFunc st)
