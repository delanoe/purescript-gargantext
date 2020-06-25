module Gargantext.Components.Nodes.Corpus.Chart.Common where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.Nodes.Corpus.Chart.Types
import Gargantext.Hooks.Loader (useLoader, useLoaderWithCache)
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
    cpt {getMetrics, loaded, path, reload, session} _ = do
      useLoader (fst reload /\ path) (getMetrics session) $ \l ->
        loaded session path reload l

type MetricsWithCacheLoadViewProps a = (
  --keyFunc :: Record Path -> String
  | MetricsLoadViewProps a
  )

metricsWithCacheLoadView :: forall a. DecodeJson a => EncodeJson a =>
                            Record (MetricsLoadViewProps a) -> R.Element
metricsWithCacheLoadView p = R.createElement metricsWithCacheLoadViewCpt p []

metricsWithCacheLoadViewCpt :: forall a. DecodeJson a => EncodeJson a => R.Component (MetricsLoadViewProps a)
metricsWithCacheLoadViewCpt = R.hooksComponent "G.C.N.C.C.metricsWithCacheLoadView" cpt
  where
    cpt {getMetrics, loaded, path, reload, session} _ = do
      useLoaderWithCache (fst reload /\ path) keyFunc (getMetrics session) $ \l ->
        loaded session path reload l
    keyFunc (_ /\ { corpusId, listId, tabType }) = "metrics-" <> (show tabType) <> "-" <> (show corpusId) <> "-" <> (show listId)
